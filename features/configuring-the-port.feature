Feature: configuring the port

  As an Exosphere developer testing my application
  I want to be able to boot ExoComm at various ports
  So that I have flexibility in testing my system.

  Rules:
  - the default port is 3100
  - provide the "--port" command-line switch to boot up at a custom port


  Scenario: running at the default port
    When I run ExoComm at the default port
    Then it runs at port 3100


  Scenario: trying to run at the default port when it is already used
    Given another service already uses port 3100
    When I try to run ExoComm at the default port
    Then it aborts with the message "port 3100 is already in use"


  Scenario: running at a custom port
    When I run ExoComm at port 3200
    Then it runs at port 3200


  Scenario: trying to run at an already used port
    Given another service already uses port 3200
    When I try to run ExoComm at port 3200
    Then it aborts with the message "port 3200 is already in use"
