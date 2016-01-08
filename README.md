# Exosphere Communication Relay for JavaScript

> Communication relay between JavaScript code bases and the Exosphere environment

[![Circle CI](https://circleci.com/gh/Originate/exorelay-js.svg?style=shield)](https://circleci.com/gh/Originate/exorelay-js)


## Usage

```javascript
ExoRelay = require('exorelay');

// Create an ExoRelay instance
exoRelay = new ExoRelay();

// register handlers for commands
exoRelay.registerHandler('add', function(payload, send) {
  send('added', payload[0] + payload[1]);
});

// bring this relay online at port 3000
exoRelay.listen(3000);
```

With the above setup, you can send commands to the relay, for example:

```
curl -X POST -H "Content-Type: application/json" -d '[1, 2]' -i http://localhost:3000/run/add
```
