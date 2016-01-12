# JavaScript Exoservice Framework

> opinionated framework for creating simple Exosphere back-end microservices in Node.js

[![Circle CI](https://circleci.com/gh/Originate/exoservice-js.svg?style=shield&circle-token=33fbf4fc2b0c128479443c5e8bff337815205ec7)](https://circleci.com/gh/Originate/exoservice-js)



## Installation

```bash
npm install -g exoservice-js
```


## Create a micro-service

1. create an empty service scaffold
  * `yo exoservice <your service name>`
  * later: `exo-js new <your service name>`

1. TDD the service
  * end-to-end tests go into `features`
  * tests are run with `cucumber-js`

1. the application code lives in `src/<your service name>-service.ls`
  * `before-all` runs at server startup
  * `before-each` runs before each command handler
  * `after-each` runs after each command handler
  * your command handlers are key-value pairs in the exported data structure,
    where the key is the command name and the value the handler function

1. start your server through `exo-js run`


## Development

See your [developer guidelines](CONTRIBUTING.md)
