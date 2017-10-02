# 0.26.4 (2017-10-02)

#### New Features

* `exocom`: forward "message unauthorized" to sender of the unauthorized message

# 0.26.3 (2017-09-22)

#### New Features

* `exorelay`: exponential backoff for cannot connect logs

#### Bug Fixes

* `exocom`: fix memory leak (introduced in support of multiple instances of a service)

# 0.26.2 (2017-09-21)

#### New Features

* `exocom`: support multiple instances of a service
* add `exosecurity` (javascript and go): to create security services

# 0.26.1 (2017-09-05)

#### New Features

* Exocom comes online at port 80 when `PORT` env var is not set, and exorelay/exoservice connect to port 80 when `EXOCOM_PORT` env var is not set

# 0.26.0 (2017-08-31)

#### BREAKING CHANGES

* `javascript/exoservice`: the executable has been removed to make the interface flexible enough to support languages that transpile to javascript.
  * Require `exoservice` in your file and use it in the following way.
    ```javascript
    const {bootstrap} = require('exoservice')

    bootstrap({
      'before-all': (done) => {
        done()
      },
      ping: (_, {reply}) => {
        reply('pong')
      }
    })
    ```
  * Run the file

#### New Features

* `exorelay` / `exoservice`: update reply handlers to retain the `isSecurity` flag
* `javascript/exorelay`: add ability to supply `auth`

# 0.25.1 (2017-08-29)

#### Bug fixes

* `javascript/exocom-lint`: fix package name

# 0.25.0 (2017-08-29)

#### BREAKING CHANGES

* message structure: change `sessionId` to `auth`
* `javascript/exorelay`: update the `send` method callback parameters to be `(messageName, payload)`

#### New Features

* add `javascript/exocom-lint`: a CLI tool for linting an exosphere application that uses exocom
* `exocom`: add security adapter functionality

#### Bug fixes

* `go/exocom`: fix race condition around registering services

# 0.24.0 (2017-08-08)

#### BREAKING CHANGES

* Update message translation to use a 1-1 mapping for message names instead of a namespace

#### Bug fixes

* `javascript/exorelay`: fix reply handlers

# 0.23.0 (2017-08-03)

#### BREAKING CHANGES

* Rewrite of `exocom-server` in go. Now lives in `go/exocom`
  * nothing is expected to be break as purely internal
  * docker image dropped from 275 MB to 4 MB
* `exocom` / `exorelay`: replace `responseTo` in favor of `activityId`
  * nothing is expected to be break as purely internal
* Remove out of date Haskell, Java, Scala `exorelay` implementations

#### New Features

* add `go/frontendbridge`
  * allows frontend clients to send messages to exocom
  * any messages sent in response to messages from a client are forwarded back to only that client
* `go/exorelay`: retry failed initial connection to exocom / reconnect if exocom goes down momentarily
* `javascript/exorelay`: attempt to reconnect if exocom goes down / reconnect if exocom goes down momentarily
* `go/exocom-mock`: add new method `AddMockReply`

#### Bug Fixes

* `exocom`
  * do not exit when sending unregistered message
  * account for undefined `sends` in routing
* `go/exorelay`: connect to the exocom `/services` instead of `/`

# 0.22.1 (2017-06-08)

No changes

# 0.22.0 (2017-06-08)

#### New Features

* Add go version of exocom-mock / exorelay / exoservice
* Add support message names with whitespace
