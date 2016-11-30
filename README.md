# Exosphere Communication Server

[![Circle CI](https://circleci.com/gh/Originate/exocom-dev.svg?style=shield&circle-token=0f68f90da677a3c5bffc88d9d41910c00f10b81e)](https://circleci.com/gh/Originate/exocom-dev)
[![Dependency Status](https://david-dm.org/originate/exocom-dev.svg)](https://david-dm.org/originate/exocom-dev)
[![devDependency Status](https://david-dm.org/originate/exocom-dev/dev-status.svg)](https://david-dm.org/originate/exocom-dev#info=devDependencies)
[![yarn compatibility](https://img.shields.io/badge/yarn-compatible-brightgreen.svg)](https://yarnpkg.com)

This is a lightweight, fast, in-memory implementation of the **Exo**sphere **Com**munication infrastructure for AI-native applications,
optimized for running easily without dependencies,
for example on developer machines,
without production-grade features like persistence or horizontal scalability.


## Features

* [broadcasting](features/broadcasting-messages.feature) of messages to other services
* optional [translating](features/translating-messages.feature) of messages along the way


## Distribution

This implementation is bundled into the
[Exosphere SDK](https://github.com/Originate/exosphere-sdk),
no need to install it separately.


## Development

[developer guidelines](CONTRIBUTING.md)
