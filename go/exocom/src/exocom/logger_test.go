package exocom_test

import (
	"io"
	"io/ioutil"

	"github.com/Originate/exocom/go/exocom/src/exocom"
	"github.com/Originate/exocom/go/exocom/src/types"
	"github.com/Originate/exocom/go/structs"
	"github.com/fatih/color"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

func GetOutput(f func(*exocom.Logger)) string {
	pipeReader, pipeWriter := io.Pipe()
	logger := exocom.NewLogger(pipeWriter)
	go func() {
		f(logger)
		err := pipeWriter.Close()
		Expect(err).To(BeNil())
	}()
	result, err := ioutil.ReadAll(pipeReader)
	Expect(err).To(BeNil())
	return string(result)
}

var _ = Describe("Logger", func() {
	Describe("Log", func() {
		It("prints the text to the screen with a new line character appended", func() {
			result := GetOutput(func(logger *exocom.Logger) {
				err := logger.Log("foo bar")
				Expect(err).To(BeNil())
			})
			Expect(result).To(Equal("foo bar\n"))
		})
	})
	Describe("Write", func() {
		It("prints the text to the screen without a new line character appended", func() {
			result := GetOutput(func(logger *exocom.Logger) {
				err := logger.Write("foo bar")
				Expect(err).To(BeNil())
			})
			Expect(result).To(Equal("foo bar"))
		})
	})
	Describe("Error", func() {
		It("prints an error", func() {
			result := GetOutput(func(logger *exocom.Logger) {
				err := logger.Error("error")
				Expect(err).To(BeNil())
			})
			Expect(result).To(Equal(color.RedString("error\n")))
		})
	})
	Describe("Header", func() {
		It("prints a header", func() {
			result := GetOutput(func(logger *exocom.Logger) {
				err := logger.Header("header")
				Expect(err).To(BeNil())
			})
			expected := color.New(color.Faint).Sprint("header\n")
			Expect(result).To(Equal(expected))
		})
	})
	Describe("Warning", func() {
		It("prints a warning", func() {
			result := GetOutput(func(logger *exocom.Logger) {
				err := logger.Warning("warning")
				Expect(err).To(BeNil())
			})
			Expect(result).To(Equal(color.YellowString("warning\n")))
		})
	})

	Describe("Messages", func() {
		Describe("outgoing message translation", func() {
			It("prints the message that is sent, the response time, and the payload", func() {
				message := structs.Message{
					Name:         "mongo.created",
					Sender:       "users",
					Payload:      map[string]interface{}{},
					ResponseTime: 1000,
				}
				receiverMapping := types.ReceiverMapping{"web": "users.created", "tweets": "users.created"}
				result := GetOutput(func(logger *exocom.Logger) {
					err := logger.Messages(message, receiverMapping)
					Expect(err).To(BeNil())
				})
				Expect(result).To(ContainSubstring(
					"users  --[ mongo.created ]-[ users.created ]->  web  ( 1µs )\n" +
						"{}\n"))
				Expect(result).To(ContainSubstring(
					"users  --[ mongo.created ]-[ users.created ]->  tweets  ( 1µs )\n" +
						"{}\n"))
			})
		})

		Describe("incoming message translation", func() {
			It("prints the message that is sent, the response time, and the payload", func() {
				message := structs.Message{
					Name:    "users.create",
					Sender:  "web",
					Payload: map[string]interface{}{},
				}
				receiverMapping := types.ReceiverMapping{"users": "mongo.create"}
				result := GetOutput(func(logger *exocom.Logger) {
					err := logger.Messages(message, receiverMapping)
					Expect(err).To(BeNil())
				})
				Expect(result).To(Equal(
					"web  --[ users.create ]-[ mongo.create ]->  users\n" +
						"{}\n"))
			})
		})

		Describe("no message translation", func() {
			It("prints the message that is sent, the response time, and the payload", func() {
				message := structs.Message{
					Name:    "users.create",
					Sender:  "web",
					Payload: map[string]interface{}{},
				}
				receiverMapping := types.ReceiverMapping{"users": "users.create"}
				result := GetOutput(func(logger *exocom.Logger) {
					err := logger.Messages(message, receiverMapping)
					Expect(err).To(BeNil())
				})
				Expect(result).To(Equal(
					"web  --[ users.create ]->  users\n" +
						"{}\n"))
			})
		})
	})
})
