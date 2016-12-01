# ExoCom Developer Guidelines

This monorepository contains the different subprojects for it:

* __exocom-server:__ the actual bus implementation
* __exocom-mock-*:__ mock implementations of the ExoCom server
                     for testing
* __exorelay-*:__ client SDKs for writing services that talk to ExoCom
* __exoservice-*:__ full-stack frameworks
                    for writing micro-services
                    as pure business logic,
                    i.e. as _lambda functions_


* set up dev environment:
  * install [Morula](https://github.com/Originate/morula)
  * run `morula all bin/setup`

* run tests for all subprojects:

  ```
  $ morula all bin/spec
  ```

* run tests for changed subprojects (when on a feature branch):

  ```
  $ morula changed bin/spec
  ```
