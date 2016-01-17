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

```coffeescript
ExoRelay = require 'exorelay'

exoRelay = new ExoRelay()
exoRelay.listen()
```

More details and how to customize the port is described in the [spec](features/listen.feature).


## Handle incoming commands

Register a handler for incoming commands:

```coffeescript
exoRelay.registerHandler 'hello', (name) ->
  console.log "Hello #{name}"
```

More details on how to define command listeners are [here](features/receiving-commands.feature).
If you are implementing services, you want to send outgoing replies to incoming commands:

```coffeescript
exoRelay.registerHandler 'users.create', (userData, {reply}) ->
  # on this line we would create a user database record with the attributes given in userData
  reply 'users.created', id: 456, name: userData.name
```

More details and a working example of how to send replies is [here](features/outgoing-replies.feature).



## Send outgoing commands

Send a command to Exosphere:

```coffeescript
exoRelay.send 'hello', name: 'world'
```

Sending a command is fire-and-forget, i.e. you don't have to wait for the
sending process to finish before you can do the next thing.
More details on how to send various data are [here](features/sending.feature).

You can handle the incoming replies to your outgoing commands:

```coffeescript
exo-relay.send 'users.create', name: 'Will Riker', (createdUser) ->
  print "created user #{createdUser.id}"
```

More examples for handling incoming replies are [here](features/incoming-replies.feature).
Command handlers also provide a shortcut to send commands:

```coffeescript
exoRelay.registerHandler 'users.create', (userData, {send, reply}) ->
  send 'passwords.encrypt' userData.password, (encryptedPassword) ->
    userData.encryptedPassword = encryptedPassword
    # on this line we would create a user database record with the attributes given in userData
    reply 'users.created', id: 456, name: userData.name
```

More details and a working example of how to send commands from within command handlers is [here](features/sending-from-commands.feature).
