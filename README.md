# ExoCom
> A modern message bus for AI-native application ecosystems

[![CircleCI](https://circleci.com/gh/Originate/exocom/tree/master.svg?style=shield)](https://circleci.com/gh/Originate/exocom/tree/master)

This monorepository contains the different subprojects for ExoCom:

* __server:__ [exocom-server](exocom-server) contains the actual bus implementation,
  _exocom-mock-*_ are mock implementations for testing in different languages

* __clients:__ are called _exorelays_.
  Implementations are available for a variety of languages.

* __services:__ are full-stack frameworks that allow to define micro-services
                as pure business logic, i.e. as _lambda functions_.
                Implementations are available for a variety of languages.


## Development

See our [developer guidelines](CONTRIBUTING.md)
