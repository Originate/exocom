# Mock implementation of ExoComm in JavaScript

[![Circle CI](https://circleci.com/gh/Originate/exocomm-mock-js.svg?style=shield&circle-token=4f522d83e80f98f58b30cd1c9ad7f2e24f8e0b58)](https://circleci.com/gh/Originate/exocomm-mock-js)

> a mock implementation of [ExoComm-Dev](https://github.com/Originate/exocomm-dev)
for sending and receiving commands to your ExoServices in test


## Installation

```
$ npm i --save-dev exocomm-mock
```


## Usage

* create an instance

  ```coffeescript
  ExoCommMock = require('exocomm-mock')
  exocomm = new ExoCommMock
  ```

* register a service to send commands to

  ```coffeescript
  exocomm.registerService name: 'users', port: 4001
  ```

* send a command to the service

  ```coffeescript
  exocomm.sendCommand service: 'users', name: 'users.create', payload: { name: 'Jean-Luc Picard' }
  ```

* verifying commands sent out by the service under test

  ```coffeescript
  # ... make your service sent out a request here via exocomm.sendCommand...

  # wait for the command to arrive
  exocomm.waitUntilReceive =>

    # verify the received command
    expect(exocomm.receivedCommands()).to.eql [
      {
        name: 'users.created'
        payload:
          name: 'Jean-Luc Picard'
      }
    ]
  ```

* if you want to verify more received commands later, you can reset the register of received commands so far

  ```coffeescript
  exocomm.reset()
  ```

* finally, close your instance when you are done, so that you can create a fresh one for your next test

  ```coffeescript
  exocomm.close()
  ```


## Development

See our [developer documentation](CONTRIBUTING.md)
