Feature: Defining the port at which the server listens

  As an Exosphere developer
  I want to be able to run my service at a configurable port
  So that my services fit seamlessly into the networking setup of my infrastructure.


  Rules:
  - call "exo-js run" in the directory of an Exosphere service
    to run the service at the default port 3000
  - the port can be customized via the "--port" command-line switch

  Notes:
  - in production there might be other ways to subscribe to commands
    and pull them down when the service has capacity to do so


  Background:
    Given I am in the "hello-world" service directory


  Scenario: Running with default options
    When executing "exo-js run"
    Then the service runs at port 3000


  Scenario: Running at a custom port
    When executing "exo-js run --port 3001"
    Then the service runs at port 3001

