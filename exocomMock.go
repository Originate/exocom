package exocomMock

import (
	"fmt"
	"log"
	"net/http"

	"golang.org/x/net/websocket"
)

type ExoCom struct {
	ServerPort int
	Services   map[string]websocket.Conn
	done       chan bool
}

type Message struct {
	Sender       string `json:"sender"`
	Name         string `json:"name"`
	Payload      string `json:"payload"`
	ResponseTo   string `json:"responseTo"`
	ID           string `json:"id"`
	Timestamp    int    `json:"timestamp"`
	ResponseTime int    `json:"timestamp"`
}

type Service struct {
	name string
	id   int
	ws   *websocket.Conn
}

func New() *ExoCom {
	http.Handle("/services", websocket.Handler(onConnected))
	log.Println("EXOCOM: ExoCom INITIALIZED!!!!")
	doneChannel := make(chan bool)
	go func() {
		select {}
	}()
	return &ExoCom{0, nil, doneChannel}
}

func (exocom *ExoCom) Close() {

}

func (exocom *ExoCom) Listen(port int) {
	log.Println("EXOCOM: Starting listener.")
	exocom.ServerPort = port
	err := http.ListenAndServe(fmt.Sprintf("localhost:%d", port), websocket.Handler(onConnected))
	if err != nil {
		log.Fatalln(err)
	}
	log.Println("EXOCOM: Listener is done")
}

func onConnected(ws *websocket.Conn) {
	var message Message
	err := websocket.JSON.Receive(ws, &message)
	log.Println("EXOCOM: CLIENT CONNECTED!!!! YAY!!!!\nEXOCOM: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
	if err != nil {
		log.Fatalln(err)
	} else {
		fmt.Println(message)
	}
}

// Service methods
func (service *Service) New(name string, id int, ws *websocket.Conn) *Service {
	return &Service{name, id, ws}
}
