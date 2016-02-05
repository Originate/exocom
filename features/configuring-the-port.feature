Feature: Defining the port at which the server listens

  As an ExoService developer
  I want to be able to run my service at a configurable port
  So that my services fit seamlessly into the networking setup of my infrastructure.


  Rules:
  - call "exo-js run" in the directory of an Exosphere service
    to run the service at the default port 3000
  - if port 3000 is taken, it chooses the next available port above that
  - the port can be customized via the "--port" command-line switch


  Background:
    Given an ExoComm instance


  Scenario: Running with default options
    When starting a service
    Then the service runs at port 3000


  Scenario: Finding the next available port if 3000 is taken
    Given ports 3000 and 3001 are used
    When starting a service
    Then the service runs at port 3002


  Scenario: Running at a custom port
    When starting a service at port 3001
    Then the service runs at port 3001
