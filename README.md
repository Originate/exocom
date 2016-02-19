# JavaScript Exoservice Framework

> opinionated framework for creating
[Exosphere lambda services](https://github.com/Originate/exosphere/blob/master/documentation/services.md#lambda-services)
in Node.JS

[![Circle CI](https://circleci.com/gh/Originate/exoservice-js.svg?style=shield&circle-token=33fbf4fc2b0c128479443c5e8bff337815205ec7)](https://circleci.com/gh/Originate/exoservice-js)


## Installation

```bash
npm install -g exoservice-js
```


## Creating a micro-service

Let's create the simplest possible microservice:
when receiving the message "ping", it replies with the message "pong".

1. create an empty service scaffold
  * `yo exoservice ping`

1. start your server: `exo-js run --name 'pinger' --exorelay-port 3000 --exocomm-port 3100`
  * `name` is as what your service should identify to Exosphere
  * `exorelay-port` is the port at which your service listens to Exosphere messages
  * `exocomm-port` is the port at which your local ExoComm instance runs


## Testing

Test a running service manually:

```bash
$ curl -d '{"requestId": "123" }' -H "Content-Type: application/json" -i http://localhost:3000/run/hello-world
```


## Development

See your [developer guidelines](CONTRIBUTING.md)
