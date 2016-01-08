# Exosphere Communication Relay for JavaScript

This modules allows JavaScript code bases to communicate with their Exosphere
environment.


## Usage

```javascript
ExoRelay = require('exorelay');

// Create an ExoRelay instance
exoRelay = new ExoRelay();

// register a handler for a command
exoRelay.register-handler('hello', function(payload, send) {
  console.log('handling command "hello"');
});

// bring the relay online at port 3000
exoRelay.listen(3000);
```

With the above setup, you can send commands to the relay, for example:

```
curl -X POST -i http://localhost:3000/run/hello
```
