# ExoCom Developer Guidelines

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
