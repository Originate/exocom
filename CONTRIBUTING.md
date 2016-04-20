# ExoCom Guidelines

## APIs

Exocom can be used via 2 separate APIs.


### Command-line API

This is the main way to use ExoComDev by end users (Exosphere developers).

```
$ exocom
```

The default port is 3100. To run it at another port:

```
$ exocom --port <port number>
```

More details around configuring the port [here](features/configuring-the-port.feature).


### JavaScript API

You can integrate Exocom into your own NodeJS application through its JavaScript API.
The [CLI](src/cli.ls) is implemented using the JavaScript API.

```livescript
new ExoCom
  ..on 'error', (err) -> # boo!
  ..on 'listening', (port) -> # woohoo!
  ..listen 3100
```


## Functionality

## Architecture

ExoCom is implemented using the _micro-kernel_ pattern,
i.e. as a number of relatively independent subsystems that are integrated through
a relatively small and lightweight core that only provides
communication between the subsystems.
Each subsystem provides a particular set of functionality,
and is tested and implemented independently,
as a set of one or more classes.

- [HttpListener](src/http-listener): implements the HTTP endpoint that services talk to
                                     in order to make requests to ExoCom
- [ClientRegistry](src/client-registry): keeps track of which service is allowed
                                         to send and receive what messages
- [MessageSender](src/message-sender): sends messages to external services
- [Kernel](src/exocom.ls): integrates all the above subsystems and provides the [programmatic API](#javascript-api)


## Testing

The tests run against the compiled output, so you need to run `watch` before executing them.

- run all tests: `spec`
- run unit tests: `tests`
- run linter: `lint`
- run JS-API tests: `cuc_api`
- run CLI tests: `cuc_cli`


## Update dependencies


```
$ update
```


## Publish a new version

```
$ publish <patch|minor|major>
```
