import Promise from 'bluebird'
import { When } from 'cucumber'
import MockExoCom from '../../../exocom-mock'

When('ExoSocket connects to ExoCom', async function() {
  this.exoComMockInstance = new MockExoCom()
  this.exoComMockInstance.listen(this.exocomPort)
  await this.exoSocketInstance.connect()
  await new Promise(resolve => {
    this.exoComMockInstance.waitUntilKnowsService(this.role, resolve)
  })
})

When('ExoSocket boots up a second before ExoCom', async () => {
  // try {
  //   this.exoSocketInstance.connect()
  // } catch (e) {
  //   console.log('caught', e)
  // }
  // console.log('delay')
  // await Promise.delay(200)
  // console.log('delay done')
  // this.exoComMockInstance.listen(this.exocomPort)
  // console.log('waiting')
  // await this.exoComMockInstance.waitUntilKnowsService(this.role)
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

When('receiving this message:', function(docString, callback) {
  this.exoComMockInstance.send(dock)
  // Write code here that turns the phrase above into concrete actions
  callback(null, 'pending')
})
