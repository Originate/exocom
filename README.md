# Exosphere Communication Relay for JavaScript

> Communication relay between JavaScript code bases and the Exosphere environment

[![Circle CI](https://circleci.com/gh/Originate/exorelay-js.svg?style=shield&circle-token=012a2c6405c702e0a8271de804eed0c4c179772f)](https://circleci.com/gh/Originate/exorelay-js)

This library allows you to add Exosphere communication to any Node.js codebase.
It is intended to be used in your web or API server.
If you want to write a micro-service in Node,
please use [ExoService-JS](https://github.com/Originate/exoservice-js),
which uses this library internally.


## Add an ExoRelay to your application

Each code base should have only one ExoRelay instance.
ExoRelay instances emit events to signal state changes:
* __online__: the instance is completely online now. Provides the port it listens on.
* __offline__: the instance is offline now
* __error__: an error has occurred. The instance is in an invalid state,
             your application should crash.

```coffeescript
ExoRelay = require 'exorelay'

exoRelay = new ExoRelay exocommPort: <port>, serviceName: <name of the service using ExoRelay>
exoRelay.on 'online', (port) ->  # yay, we are online!
exoRelay.on 'error', (err) ->    # examine, print, or log the error here
exoRelay.listen 4000
```

More details and how to customize the port is described in the [spec](features/listen.feature).


## Handle incoming messages

Register a handler for incoming messages:

```coffeescript
exoRelay.registerHandler 'hello', (name) ->
  console.log "Hello #{name}"
```

More details on how to define message listeners are [here](features/receiving-messages.feature).
If you are implementing services, you want to send outgoing replies to incoming messages:

```coffeescript
exoRelay.registerHandler 'user.create', (userData, {reply}) ->
  # on this line we would create a user database record with the attributes given in userData
  reply 'user.created', id: 456, name: userData.name
```

More details and a working example of how to send replies is [here](features/outgoing-replies.feature).



## Send outgoing messages

Send a message to Exosphere:

```coffeescript
exoRelay.send 'hello', name: 'world'
```

Sending a message is fire-and-forget, i.e. you don't have to wait for the
sending process to finish before you can do the next thing.
More details on how to send various data are [here](features/sending.feature).


## Handle incoming replies

If a message you send expects a reply,
you can provide the handler for it right when you send it:

```coffeescript
exoRelay.send 'users.create', name: 'Will Riker', (createdUser) ->
  print "created user #{createdUser.id}"
```

Service calls are more expensive than in-process function calls.
They are also higher-level, crossing functional boundaries within your application.
Hence they (should) have more complex APIs than function calls.

* replies to commands often return the state changes caused by the command,
  to avoid having to do another call to the service to query the new state
* commands often have more than one outcome.
  For example, the command
  _"transfer $100 from the checking account to the savings account"_
  sent to an accounting service can reply with:

  <table>
    <tr>
      <th>transferred</th>
      <td>the money was transferred</td>
    </tr>
    <tr>
      <th>pending</th>
      <td>the transfer was initiated, but is pending a third-party approval</td>
    </tr>
    <tr>
      <th>transaction limit exceeded</th>
      <td>the account doesn't allow that much money to be transferred at once</td>
    </tr>
    <tr>
      <th>daily limit exceeded</th>
      <td>the daily transaction limit was exceeded</td>
    </tr>
    <tr>
      <th>insufficient funds</th>
      <td>there isn't enough money in the checking account</td>
    </tr>
    <tr>
      <th>unknown account</th>
      <td>one of the given accounts was not found</td>
    </tr>
    <tr>
      <th>unauthorized</th>
      <td>the currently logged in user does not have privileges to make this transfer</td>
    </tr>
    <tr>
      <th>internal error</th>
      <td>an internal error occurred in the accounting service</td>
    </tr>
  </table>

The outcome is provided as part of the optional second parameter to the reply handler.

```livescript
exoRelay.send 'transfer', amount: 100, from: 'checking', to: 'savings', (txn, {outcome}) ->
  switch outcome
    | 'transferred'  =>  ...
    | 'pending'      =>  ...
    | ...
```

A different use case for checking outcomes is ongoing monitoring of commands
that take a while to execute.
A service can send multiple replies, causing the reply handler to be called
multiple times. Each reply can be a different message type:

```livescript
exoRelay.send 'file.copy', from: 'large.csv', to: 'backup.csv', (payload, {outcome}) ->
  switch outcome
    | 'file.copying'  =>  console.log "still copying, #{payload.percent}% done"
    | 'file.copied'   =>  console.log 'file copy finished!'
```

Another use case is streaming responses, where a larger result is sent in chunks:

```livescript
exoRelay.send 'file.read', path: 'large.csv', (payload, {outcome}) ->
  switch outcome
    | 'file.read-chunk'  =>  result += payload
    | 'file.read-done'   =>  console.log "finished reading #{payload.megabytes} MB!"
```

More examples for handling incoming replies are [here](features/incoming-replies.feature).
Message handlers also provide a shortcut to send messages:

```coffeescript
exoRelay.registerHandler 'users.create', (createdUser, {send}) ->
  send 'passwords.encrypt' createdUser.password, (encryptedPassword) ->
    ...
```

More details and a working example of how to send messages from within message handlers is [here](features/sending-from-messages.feature).


## Development

See our [developer guidelines](CONTRIBUTING.md)
