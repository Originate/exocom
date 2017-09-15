const { expect } = require('chai')
const { Then } = require('cucumber')

Then(/^it sends the message:$/, function(message, done) {
  this.exocom.onReceive(() => {
    const receivedMessage = this.exocom.receivedMessages[0]
    const replyMessage = JSON.parse(message)
    replyMessage.id = receivedMessage.id
    if (
      replyMessage.payload &&
      replyMessage.payload.activityId &&
      replyMessage.payload.id
    ) {
      replyMessage.payload.activityId = receivedMessage.payload.activityId
      replyMessage.payload.id = receivedMessage.payload.id
    }
    expect(receivedMessage).to.deep.equal(replyMessage)
    done()
  })
})
