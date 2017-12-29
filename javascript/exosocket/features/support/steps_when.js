import Promise from 'bluebird'
import { When } from 'cucumber'
import MockExoCom from '../../../exocom-mock'

When('ExoSocket connects to ExoCom', async function() {
  this.exoComMockInstance = new MockExoCom()
  this.connectInstances()
})

When('ExoSocket boots up a second before ExoCom', async function() {
  this.exoSocketInstance.connect()
  await Promise.delay(200)
  this.exoComMockInstance = new MockExoCom()
  this.exoComMockInstance.listen(this.exocomPort)
  await this.exoComMockInstance.waitUntilKnowsService(this.role)
})

When('sending the message {string}', function(messageName) {
  this.exoSocketInstance.send({ name: messageName })
})

When('sending the message {string} with the payload:', function(
  messageName,
  docString
) {
  this.exoSocketInstance.send({
    name: messageName,
    payload: JSON.parse(docString),
  })
})

When('trying to send an empty message', function() {
  try {
    this.exoSocketInstance.send({})
  } catch (e) {
    this.error = e
  }
})

When('sending the message {string} with auth {string}', function(
  messageName,
  auth
) {
  this.exoSocketInstance.send({
    name: messageName,
    auth,
  })
})

When('receiving this message:', function(docString) {
  this.exoComMockInstance.send(this.role, JSON.parse(docString))
})

When('Exocom crashes and reboots', async function() {
  await this.exoComMockInstance.close()
  this.exoComMockInstance = new MockExoCom()
  this.exoComMockInstance.listen(this.exocomPort)
})
