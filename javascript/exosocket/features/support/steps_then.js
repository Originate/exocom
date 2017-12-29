import { expect } from 'chai'
import { Then } from 'cucumber'
import Mustache from 'mustache'

Then(
  'it registers by sending the message {string} with the sender {string}',
  async function(messageName, sender) {
    await this.waitForMessageCount(1, 2000)
    const message = this.exoComMockInstance.receivedMessages[0]
    expect(message.name).to.eql(messageName)
    expect(message.sender).to.eql(sender)
  }
)

Then('ExoSocket should connect to ExoCom', function() {
  return this.waitForConnection()
})

Then('ExoSocket makes the WebSocket request:', async function(docString) {
  await this.waitForMessageCount(2, 2000)
  const actualMessage = this.exoComMockInstance.receivedMessages[1]
  const expectedMessage = JSON.parse(
    Mustache.render(docString, {
      outgoingMessageId: actualMessage.id,
      outgoingActivityId: actualMessage.activityId,
    })
  )
  expect(actualMessage).to.eql(expectedMessage)
})

Then('ExoSocket errors with {string}', function(message) {
  expect(this.error.message).to.eql(message)
})

Then(
  'the fixture receives a message with the name {string} and no payload',
  async function(messageName) {
    const actualMessage = await this.getTestFixtureReceivedMessage()
    expect(actualMessage.name).to.eql(messageName)
    expect(actualMessage.payload).to.eql(undefined)
  }
)

Then(
  'the fixture receives a message with the name {string} and the payload:',
  async function(messageName, docString) {
    const actualMessage = await this.getTestFixtureReceivedMessage()
    expect(actualMessage.name).to.eql(messageName)
    expect(actualMessage.payload).to.eql(JSON.parse(docString))
  }
)

Then(
  'the fixture receives a message with the name {string} and auth {string}',
  async function(messageName, auth) {
    const actualMessage = await this.getTestFixtureReceivedMessage()
    expect(actualMessage.name).to.eql(messageName)
    expect(actualMessage.auth).to.eql(auth)
  }
)

Then(
  'the fixture receives a message with the name {string} and isSecurity true',
  async function(messageName) {
    const actualMessage = await this.getTestFixtureReceivedMessage()
    expect(actualMessage.name).to.eql(messageName)
    expect(actualMessage.isSecurity).to.eql(true)
  }
)

Then('ExoSocket should reconnect to ExoCom', function() {
  return this.waitForConnection()
})
