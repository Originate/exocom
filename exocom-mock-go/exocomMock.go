package exocomMock

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"sync"
	"time"

	graceful "gopkg.in/tylerb/graceful.v1"

	"github.com/fatih/color"

	"golang.org/x/net/websocket"
)

var red = color.New(color.FgRed).SprintFunc()
var cyan = color.New(color.FgHiCyan).SprintFunc()

type ExoCom struct {
	sync.Mutex
	server           *graceful.Server
	port             int
	Services         map[string]*websocket.Conn
	ReceivedMessages []Message
	doneCh           chan bool
	messageCh        chan Message
	errCh            chan error
}

type Message struct {
	Name         string      `json:"name,omitempty"`
	Sender       string      `json:"sender,omitempty"`
	Payload      interface{} `json:"payload,omitempty"`
	ResponseTo   string      `json:"responseTo,omitempty"`
	ID           string      `json:"id,omitempty"`
	Timestamp    int         `json:"timestamp,omitempty"`
	ResponseTime int         `json:"timestamp,omitempty"`
}

func New() *ExoCom {
	log.Println("EXOCOM: ExoCom initialized!")
	return &ExoCom{
		port:             0,
		Services:         make(map[string]*websocket.Conn),
		ReceivedMessages: make([]Message, 0),
		doneCh:           make(chan bool, 1),
		messageCh:        make(chan Message),
		errCh:            make(chan error),
	}
}

func (exocom *ExoCom) RegisterService(name string, ws *websocket.Conn) {
	exocom.Services[name] = ws
}

func (exocom *ExoCom) Close() {
	exocom.doneCh <- true
	exocom.server.Stop(100 * time.Millisecond)
}

func (exocom *ExoCom) listenToMessages(ws *websocket.Conn) {
	go exocom.messageHandler(ws)
	for {
		select {
		case <-exocom.doneCh:
			exocom.doneCh <- true
			return
		case incoming := <-exocom.messageCh:
			log.Printf(cyan("EXOCOM: MESSAGE RECEIVED in listenToMessages: %#v\n"), incoming)
		}
	}
}

func (exocom *ExoCom) messageHandler(batman *websocket.Conn) {
	var incoming Message
	for {
		select {
		case <-exocom.doneCh:
			exocom.doneCh <- true
			return
		default:
			err := websocket.JSON.Receive(batman, &incoming)
			if err == io.EOF {
				return
			}
			exocom.Lock()
			exocom.saveMessage(incoming)
			exocom.Unlock()
			exocom.messageCh <- incoming
		}
	}
}

func (exocom *ExoCom) saveMessage(message Message) {
	exocom.ReceivedMessages = append(exocom.ReceivedMessages, message)
}

func (exocom *ExoCom) Listen(port int) {
	exocom.port = port

	onConnection := func(ws *websocket.Conn) {
		//ws.SetReadDeadline(time.Now().Add(1 * time.Second))
		var incoming Message
		if err := websocket.JSON.Receive(ws, &incoming); err != nil {
			log.Fatal(red(err))
		}
		if incoming.Name == "exocom.register-service" {
			exocom.RegisterService(incoming.Sender, ws)
			exocom.saveMessage(incoming)
			exocom.listenToMessages(ws)
		}
	}

	exocom.server = &graceful.Server{
		Timeout: 1 * time.Second,
		Server: &http.Server{
			Addr:    fmt.Sprintf(":%d", port),
			Handler: websocket.Handler(onConnection),
		},
	}
	err := exocom.server.ListenAndServe()
	if err != nil {
		log.Fatalln(red(err))
	}
}
