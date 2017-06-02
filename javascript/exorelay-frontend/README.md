# Exosphere Communication Relay for JavaScript

> Communication relay between JavaScript frontends and an Exosphere backend

This library allows you to add Exosphere communication to a JavaScript frontend.

## Add a exorelay-frontend to your application

```coffeescript
ExoRelay = require 'exorelay'

exoRelay = new ExoRelay host: <host>, port: <port>, serviceName: <name of the service using ExoRelay>
exoRelay.on 'online', (port) ->  # yay, we are online!
exoRelay.on 'error', (err) ->    # examine, print, or log the error here
exoRelay.listen 4000
```


## Handle incoming messages

```coffeescript
exoRelay.registerHandler 'hello', (name) -> console.log "hello #{name}!"
```

## Send outgoing messages

```coffeescript
exoRelay.send 'hello', name: 'world'
```

## Send outgoing replies to incoming messages

If you are implementing services, you want to send outgoing replies to incoming messages:

```coffeescript
exoRelay.registerHandler 'user.create', (userData, {reply}) ->
  # on this line we would save userData in the database
  reply 'user.created', id: 456, name: userData.name
```

## Handle incoming replies

If a message you send expects a reply,
you can provide the handler for it right when you send it:

```coffeescript
exoRelay.send 'users.create', name: 'Will Riker', (createdUser) ->
  console.log "the remove service has finished creating user #{createdUser.id}"
```

## Development

See our [developer guidelines](CONTRIBUTING.md)
