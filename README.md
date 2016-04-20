# Exosphere Communication Server

[![Circle CI](https://circleci.com/gh/Originate/exocom-dev.svg?style=shield&circle-token=0f68f90da677a3c5bffc88d9d41910c00f10b81e)](https://circleci.com/gh/Originate/exocom-dev)

This is a lightweight, fast, in-memory implementation of the **Exo**sphere **Com**munication infrastructure,
optimized for running easily without dependencies,
for example on developer machines,
without production-grade features like persistence or horizontal scalability.


## Features

* [broadcasting](features/broadcasting-messages.feature) of messages to other services
* optional [translating](features/translating-messages.feature) of messages along the way


## Architecture

<img src="documentation/architecture.gif">


## Distribution

This implementation is bundled into the
[Exosphere SDK](https://github.com/Originate/exosphere-sdk),
no need to install it separately.


## Development

[developer guidelines](CONTRIBUTING.md)
