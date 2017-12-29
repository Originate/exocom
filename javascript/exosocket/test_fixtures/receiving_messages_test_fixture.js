export default class ReceivingMessagesTestFixture {
  constructor() {
    this.receivedMessages = []
  }

  setup(exoSocket) {
    exoSocket.on('message', message => {
      this.receivedMessages.push(message)
    })
  }
}
