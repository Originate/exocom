const uuid = require('uuid')
const { EventEmitter } = require('events')
const ExoRelay = require('../../exorelay')
const { delegate, delegateEvent } = require('rails-delegate')

class Exosecurity extends EventEmitter {
  constructor({ exocomHost, exocomPort, role }) {
    super()
    this.exoRelay = new ExoRelay({
      exocomHost,
      exocomPort,
      role,
    })
    delegate('close', {
      from: this,
      to: this.exoRelay,
    })
    delegateEvent('online', 'offline', 'error', {
      from: this.exoRelay,
      to: this,
    })
  }

  buildRequester(activityId) {
    return (name, payload, callback) => {
      const request = { name, payload, activityId: uuid.v1(), id: uuid.v1() }
      this.exoRelay.send(
        'security request',
        request,
        { activityId },
        (messageName, messagePayload) => {
          if (messageName === 'security response') {
            callback(messagePayload.name, messagePayload.payload)
          } else {
            console.log('unexpected response to security request', messageName) // eslint-disable-line no-console
          }
        }
      )
    }
  }

  connect(messageAuthorizer) {
    this.exoRelay.connect()
    this.exoRelay.registerHandlers({
      'authorize message': (message, { reply }, activityId) =>
        messageAuthorizer(message, this.buildRequester(activityId), result => {
          const replyMessage = result
            ? 'message authorized'
            : 'message unauthorized'
          reply(replyMessage)
        }),
    })
  }
}
module.exports = Exosecurity
