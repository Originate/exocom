# Exosphere Communication Relay for JavaScript

> Communication relay between JavaScript code bases and the Exosphere environment

[![Circle CI](https://circleci.com/gh/Originate/exorelay-js.svg?style=shield)](https://circleci.com/gh/Originate/exorelay-js)


## Starting a relay

```javascript
ExoRelay = require('exorelay');

// Create an ExoRelay instance
exoRelay = new ExoRelay();

// bring this relay online
exoRelay.listen();
```

more details in the [spec](features/listen-at-port.feature)


## Listening to incoming commands

You can register a handler for incoming commands like this:

```javascript
// register handlers for commands
exoRelay.registerHandler('add', function(payload, send) {
  send('added', payload[0] + payload[1]);
});
```

With the above setup, you can send commands to the relay, for example:

```
curl -X POST -H "Content-Type: application/json" -d '[1, 2]' -i http://localhost:4000/run/add
```

more info in the [spec](features/command-handlers.feature)


## Sending outgoing commands

* sending an outgoing command:

  ```javascript
  exoRelay.send(command: 'hello-world', done);
  ```

* attaching payload to the sent command:

  ```javascript
  exoRelay.send({ command: 'hello', payload: { name: 'world' }}, done);
  ```

* mentioning that this command is a reply to a previously received command with id "123":

  ```javascript
  exoRelay.send({ command: 'hello', "reply-to": "123", done);
  ```
