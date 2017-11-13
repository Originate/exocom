import { EventEmitter } from 'events'
import uuid from 'uuid'

export default class ExoSocket extends EventEmitter {
  constructor({ exocomHost, exocomPort, role }) {
    super()
    this.exocomHost = exocomHost
    this.exocomPort = exocomPort
    this.role = role
  }

  // Public functions

  connect() {
    this.startConnectTime = Date.now()
    this.logConnectErrorDelay = 1000
    this.shouldReconnectOnSocketClosed = true
    this.shouldUseInternalConnect = true
    this.internalConnect()
  }

  close(done) {
    if (!this.socket) {
      done()
      return
    }
    this.shouldReconnectOnSocketClosed = false
    switch (this.socket.readyState) {
      case WebSocket.CONNECTING: {
        this.socket.terminate()
        done()
        return
      }
      case WebSocket.OPEN: {
        this.socket.on('close', done)
        this.socket.close()
        return
      }
      case WebSocket.CLOSING: {
        this.socket.on('close', done)
        return
      }
      default: {
        done()
      }
    }
  }

  send(input) {
    if (!this.socket) {
      throw new Error('Not connected to Exocom')
    }
    if (!input.name) {
      throw new Error('Message must have a name')
    }
    const message = { ...input, id: uuid.v1(), sender: this.role }
    if (!message.activityId) {
      message.activityId = uuid.v1()
    }
    this.socket.send(JSON.stringify(message))
  }

  // Private functions

  internalConnect() {
    this.socket = new WebSocket(
      `ws://${this.exocomHost}:${this.exocomPort}/services`
    )
    this.socket
      .on('close', this.onSocketClose)
      .on('error', this.onSocketError)
      .on('message', this.onSocketMessage)
      .on('open', this.onSocketOpen)
  }

  onSocketClose = () => {
    if (this.shouldReconnectOnSocketClosed) {
      if (this.shouldUseInternalConnect) {
        this.internalConnect()
      } else {
        this.connect()
      }
    } else {
      this.emit('offline')
    }
  }

  onSocketError = error => {
    if (error.errno === 'EADDRINUSE') {
      this.emit('error', `port ${this.exocomPort} is already in use`)
    } else if (this.socket.readyState === WebSocket.CONNECTING) {
      if (Date.now() > this.startConnectTime + this.logConnectErrorDelay) {
        this.emit(
          'error',
          `Could not connect after ${this.logConnectErrorDelay}ms: ${
            error.message
          }`
        )
        this.logConnectErrorDelay = this.logConnectErrorDelay * 2
      }
    } else {
      this.emit('error', error)
    }
  }

  onSocketMessage = data => {
    this.emit('message', JSON.parse(data))
  }

  onSocketOpen = () => {
    this.shouldUseInternalConnect = false
    this.emit('online')
    this.send({ name: 'exocom.register-service' })
  }
}
