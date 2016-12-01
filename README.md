<img src="documentation/logo.png" width="568" height="111" alt="logo">

[![Build Status](https://travis-ci.org/Originate/exocom.svg?branch=master)](https://travis-ci.org/Originate/exocom)

ExoCom is a modern message bus for AI-native application ecosystems.

- __AI-native__<br>
  ExoCom provides infrastructure and conventions
  for high-level interaction between the different parts of an application
  in the language of the domain.
  Over time, the stream of messages on it
  forms a collaborative, real-time data base
  containing the entire application state and activities in a readily understandable and consumable format.
  Tapping into this information stream,
  AI, data science, operate, and other components of the application
  can develop an understanding of what is going on.
  Insights from AI and data science are also shared on the bus,
  and adapt the application's behavior and appearance similar to how application code does.
  This seamless interaction and collaboration
  of traditional application logic and AI makes them fuse together into
  hybrid AI-native application architectures.

- __optimized for heterogenous micro-service architectures__
  - very low latency
  - built-in security best practices
  - based on open standards like [websockets](https://tools.ietf.org/html/rfc6455)
  - message translation for improved reusability of services<sup>*</sup>

- __developer friendly__
  - client SDKs available for many popular languages<sup>*</sup>
  - supports lambda functions
  - real-time inspection and tracing<sup>*</sup>
  - supports and connects micro-service architectures
    on the server, in the browser<sup>*</sup>, and in native mobile apps<sup>*</sup>


## Related projects
- [Apache Kafka](https://kafka.apache.org)
- [NATS.io](http://nats.io)
- [SenecaJS](http://senecajs.org)


## Development

See our [developer guidelines](CONTRIBUTING.md)
