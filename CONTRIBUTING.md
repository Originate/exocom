# ExoComm Guidelines


## Architecture

ExoComm is implemented using the _micro-kernel_ pattern,
i.e. as a number of relatively independent subsystems that are integrated through
a relatively small and lightweight core that only provides
communication between the subsystems.
Each subsystem provides a particular set of functionality,
and is tested and implemented independently,
as a set of one or more classes.

- [HttpListener](src/http-listener): implements the HTTP endpoint that services talk to
                                     in order to make requests to ExoComm
- [ClientRegistry](src/client-registry): keeps track of which service is allowed
                                         to send and receive what commands
- [CommandSender](src/command-sender): sends commands to external services
- [Kernel](src/exocomm.ls): integrates all the above subsystems and provides the [programmatic API](#javascript-api)


## Testing

The tests run against the compiled output, so you need to run `watch` before executing them.

- run all tests: `spec`
- run unit tests: `tests`
- run JS-API tests: `cuc_api`
- run CLI tests: `cuc_cli`
