# Exosphere Communication Relay for JavaScript

> Communication relay between JavaScript code bases and the Exosphere environment

[![Circle CI](https://circleci.com/gh/Originate/exorelay-js.svg?style=shield&circle-token=012a2c6405c702e0a8271de804eed0c4c179772f)](https://circleci.com/gh/Originate/exorelay-js)

This library allows you to add Exosphere communication to any Node.js codebase.
It is intended to be used in your web or API server.
If you want to write a micro-service in Node,
please use [ExoService-JS](https://github.com/Originate/exoservice-js),
which uses this library internally.


## Add an ExoRelay to your application

Each code base should have only one ExoRelay instance.
ExoRelay instances emit events to signal state changes:
* __online__: the instance is completely online now. Provides the port it listens on.
* __offline__: the instance is offline now
* __error__: an error has occurred. The instance is in an invalid state,
             your application should crash.

```coffeescript
ExoRelay = require 'exorelay'

exoRelay = new ExoRelay exocommPort: <port>, serviceName: <name of the service using ExoRelay>
exoRelay.on 'online', (port) ->  # yay, we are online!
exoRelay.on 'error', (err) ->    # examine, print, or log the error here
exoRelay.listen 4000
```

More details and how to customize the port is described in the [spec](features/listen.feature).


## Handle incoming messages

Register a handler for incoming messages:

```coffeescript
exoRelay.registerHandler 'hello', (name) ->
  console.log "Hello #{name}"
```

More details on how to define message listeners are [here](features/receiving-messages.feature).
If you are implementing services, you want to send outgoing replies to incoming messages:

```coffeescript
exoRelay.registerHandler 'users.create', (userData, {reply}) ->
  # on this line we would create a user database record with the attributes given in userData
  reply 'users.created', id: 456, name: userData.name
```

More details and a working example of how to send replies is [here](features/outgoing-replies.feature).



## Send outgoing messages

Send a message to Exosphere:

```coffeescript
exoRelay.send 'hello', name: 'world'
```

Sending a message is fire-and-forget, i.e. you don't have to wait for the
sending process to finish before you can do the next thing.
More details on how to send various data are [here](features/sending.feature).

You can handle the incoming replies to your outgoing messages:

```coffeescript
exoRelay.send 'users.create', name: 'Will Riker', (createdUser) ->
  print "created user #{createdUser.id}"
```

More examples for handling incoming replies are [here](features/incoming-replies.feature).
Message handlers also provide a shortcut to send messages:

```coffeescript
exoRelay.registerHandler 'users.create', (userData, {send, reply}) ->
  send 'passwords.encrypt' userData.password, (encryptedPassword) ->
    userData.encryptedPassword = encryptedPassword
    # on this line we would create a user database record with the attributes given in userData
    reply 'users.created', id: 456, name: userData.name
```

More details and a working example of how to send messages from within message handlers is [here](features/sending-from-messages.feature).


## Development

See our [developer guidelines](CONTRIBUTING.md)
