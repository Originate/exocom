# Exosphere Communication Relay for JavaScript

> Communication relay between JavaScript code bases and the Exosphere environment

[![Circle CI](https://circleci.com/gh/Originate/exorelay-js.svg?style=shield&circle-token=012a2c6405c702e0a8271de804eed0c4c179772f)](https://circleci.com/gh/Originate/exorelay-js)


## Add an Exorelay to your application

```javascript
ExoRelay = require("exorelay");

exoRelay = new ExoRelay();
exoRelay.listen();
```

More details in the [spec](features/listening.feature)


## Handle incoming commands

Register a handler for incoming commands:

```javascript
// register handlers for commands
exoRelay.registerHandler("hello-world", function() {
  // handle the command here ...
});
```

Test this setup:

```
curl -X POST -i http://localhost:4000/run/hello-world
```

More details in the [spec](features/command-handlers.feature)


## Send outgoing commands

* send an outgoing command:

  ```javascript
  exoRelay.send({command: "hello-world"}, done);
  ```

* attach payload to the sent command:

  ```javascript
  exoRelay.send({ command: "hello", payload: { name: "world" }}, done);
  ```

* mention that this command is a reply to a previously received command with id "123":

  ```javascript
  exoRelay.send({ command: "hello", "reply-to": "123", done);
  ```

More details in the [spec](features/sending-commands.feature)
