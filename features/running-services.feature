Feature: Running services

  As an Exosphere developer
  I want the Exoservice CLI to run my services in an easy way
  So that I don't have to deal with all the boilerplate of doing so myself and am productive.


  Rules:
  - call "exoservice-js run" in the directory of an Exosphere service to run it
  - the default port is 3000
  - the port can be customized via the "--port" command-line switch
  - the home page shows an HTML admin view of the service
  - commands can be sent as a POST request to "/run/<command-name>"
    with the data payload as the request body

  Notes:
  - in production there might be other ways to subscribe to commands
    and pull them down when the service has capacity to do so


  Scenario: Running with default options
    When starting the "hello-world" example application
    Then the service runs at port 3000


  Scenario: Running at a custom port
    When starting the "hello-world" example application at port 3001
    Then the service runs at port 3001


  Scenario:
