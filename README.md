# Runner for Exosphere Services written in JavaScript

This tool provides a simple and opinionated way to create
Exosphere back-end microservices in Node.js.


## Installation

```bash
npm install -g exoservice-js
```


## Create a micro-service

1. create an empty service scaffold
  * `yo exoservice <your service name>`
  * or: `exoservice-js new <your service name>`

1. TDD the service
  * end-to-end tests go into `features`
  * tests are run with `cucumber-js`

1. the application code lives in `src/<your service name>-service.ls`
  * `before-all` runs at server startup
  * `before-each` runs before each command handler
  * `after-each` runs after each command handler
  * your command handlers are key-value pairs in the exported data structure,
    where the key is the command name and the value the handler function

1. start your server through `exoservice-js run`


## Development

See your [developer guidelines](CONTRIBUTING.md)
