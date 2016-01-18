Feature: configuring the port

  As an Exosphere developer testing my application
  I want to be able to boot ExoComm at various ports
  So that I have flexibility in testing my system.

  Rules:
  - the default port is 3100
  - provide the "--port" command-line switch to boot up at a custom port


  Scenario: booting up at the default port
    When I run "bin/exocomm run"
    Then this service runs at port 3100


  Scenario: the default port is already used
    Given another service already uses port 3100
    When I run "bin/exocomm run"
    Then it aborts


  Scenario: booting up at a custom port
    When I run "bin/exocomm run --port 3200"
    Then this service runs at port 3200
