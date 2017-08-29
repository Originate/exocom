# 0.25.0 (2017-08-29)

#### BREAKING CHANGES

* change `sessionId` to `auth`
* `js/exorelay`: update the `send` method callback parameters to be `(messageName, payload)`

#### New Features

* Add `exocom-lint`

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
