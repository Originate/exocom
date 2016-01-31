# ExoComm Guidelines


## APIs

This application has 2 APIs:


### Command-line API

This is the main way to use ExoCommDev by end users (ExoSphere developers).
Specified by the [end-to-end tests](features/support/e2e-world.ls).
Implemented by [cli.ls](src/cli.ls).


### JavaScript API

It is possible to integrate ExoComm into your own NodeJS application through its JavaScript API.
Specified by the [api tests](features/support/api-world.ls).
Implemented by [exocomm.ls](src/exocomm.ls).

The [CLI](#) is an example app that includes the JavaScript API.


## Subsystems

ExoComm is implemented using the micro-kernel pattern,
i.e. as a number of relatively independent subsystems that are integrated through
a relatively small and lightweight core that only provides
communication between the subsystems.
Each subsystem provides a particular set of functionality,
and is tested and implemented independently,
as a set of one or more classes.

- [HttpListener](src/http-listener): implements the HTTP endpoint that services talk to
                                     in order to make requests to ExoComm
- [ClientRegistry](src/client-registry): keeps track of which service is allowed
                                         to send and received what commands
- [CommandSender](src/command-sender): sends commands to external services
- [Kernel](src/exocomm.ls): integrates all the above subsystems and provides the programmatic API
