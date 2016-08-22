# exorelay-hs
Communication relay between Haskell code bases and the Exosphere environment


## How to Use

First, you will will need to import the module: `import Network.Exocom`

### Initialization
Then, initialize the system by calling:
```haskell
newExoRelay portNum serviceName
```
where `portNum` is an int that represents the port that the exocom service is (already) listening on and `serviceName` is a bytestring which represents the name of your service.
Returned from this function is an `IO exoRelay` object which will be needed for future sending and receiving calls to the message bus.

### Sending Messages
To send a message, encode the message and command (message name) as a bytestring and call:
```haskell
sendMsg exo cmd payload
```
where `exo` is the exorelay instance, `cmd` is the command, and `payload` is the message contents.

If you expect a reply to your sent message then call the following function instead:
```haskell
sendMsg exo cmd payload handler
```
where `exo`, `cmd`, and `payload` is the same as the previous function. `handler` is a function with a signature: `ByteString -> IO ()` where the input argument is the content that is received as a reply to your sent message. Therefore, when a reply is received, your handler is called asynchronously on the given payload. Note that your handler is called in a different thread and therefore do not perform any thread-unsafe actions without proper precautions.

### Listening to Messages
To listen for messages, simply call:
 ```haskell
registerHandler exo cmd handler
```
where `exo` is the exorelay instance, `cmd` is the command to listen for. `handler` is a function of type `ByteString -> IO ()` which executes asynchronously when the listening system receives a message which matches the given command name. The passed in bytestring parameter is the payload of the received message. Essentially, if the listening subsystem sees a message with name parameter matching `cmd` it will execute the handler on the payload of that message. Again, since the handler is executed in a separate thread please don't perform any thread-unsafe operations without proper locking.

If you would like to listen for a message and then be able to send a reply please call this function:
```haskell
registerHandlerWithReply cmd handler
```
which is very similar to `registerHandler` except that the type of `handler` is `ByteString -> IO (ByteString, ByteString)` which again executes if a message is received with name parameter matching the `cmd` of the function call. The handler then returns an IO tuple of the form `(retCmd, retPayload)` then, the system will send a reply with command `retCmd` and payload `retPayload` to the sender of the message. Again, beware of performing non-thread-safe operations in the handler

# Caveats
Although the exorelay library itself is thread-safe and you may call any of its functions from any thread many of the handlers are executed in separate threads therefore, beware of mutating any state of your application or calling non thread safe functions without employing proper locking or knowing what you are doing.

# Building
* You will need zmq installed (at least version 4) as well as having access to an exocom instance.
* Then, just run clone the repo and run cabal build (hackage package coming soon)

# Contributing
* All contributions are welcome
* Please fill out issues as you find them
* PRs are welcome and should be compared with the `master` branch
