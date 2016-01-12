# Exosphere Communication Relay for JavaScript

> Communication relay between JavaScript code bases and the Exosphere environment

[![Circle CI](https://circleci.com/gh/Originate/exorelay-js.svg?style=shield&circle-token=012a2c6405c702e0a8271de804eed0c4c179772f)](https://circleci.com/gh/Originate/exorelay-js)

This library allows you to add Exosphere communication to any Node.js codebase.
It is intended to be used in your web or API server.
If you want to write a micro-service in Node,
please use [ExoService-JS](https://github.com/Originate/exoservice-js),
which uses this library internally.


## Add an Exorelay to your application

```javascript
ExoRelay = require("exorelay");

exoRelay = new ExoRelay();
exoRelay.listen();
```

More details and how to customize the port is described in the [spec](features/listen.feature)


## Handle incoming commands

Register a handler for incoming commands:

```javascript
exoRelay.registerHandler("hello", function(payload) {
  console.log("Hello " + payload.name);
});
```

Test this setup:

```bash
$ curl -d '{"name": "Joe"}' http://localhost:4000/run/hello_name
```

More details in the [spec](features/receiving-commands.feature)


## Send outgoing commands

Send a command to Exosphere:

```javascript
exoRelay.send({ command: "hello", payload: { name: "world" }}, done);
```

More details in the [spec](features/sending-commands.feature)


## Replies to commands

Commands can be replies to other commands.
Each command has a unique _command-id_.
Other commands can reference the id of another command
to indicate that they are a reply to that command.

As an example, let's send out a "users/create" command and handle
the reply to it that the "users" service will send out:

```javascript
exoRelay.send({ command: "users/create", payload: { name: "Jean-Luc" } }, function(createdUser) {
  console.log("user " + createdUser.id + " created");
});
```

The "users" service would be implemented using the
[ExoService-JS tool](https://github.com/Originate/exoservice-js).

More details in the [spec](features/sending-commands.feature)
