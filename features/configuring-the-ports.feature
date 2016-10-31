Feature: configuring the port

  As an Exosphere developer testing my application
  I want to be able to boot ExoCom at various ports
  So that I have flexibility in testing my system.

  - the default WebSocket port is 3100
  - the default HTTP port is 3101
  - provide the "--websocket-port" command-line switch to bind websocket at a custom port
  - provide the "--http-port" command-line switch to listen for HTTP requests on a custom port


  Scenario: running at the default ports
    When I run ExoCom
    Then it opens a websocket at port 4100
    And it opens an HTTP listener at port 4101


  Scenario: the default websocket port is already taken
    Given another service already uses port 4100
    When I try to run ExoCom
    Then it aborts with the message "port 4100 is already in use"


  Scenario: the default HTTP port is already taken
    Given another service already uses port 4101
    When I try to run ExoCom
    Then it aborts with the message "port 4101 is already in use"


  Scenario: using a custom websocket port
    When starting ExoCom at websocket port 3200
    Then it opens a websocket at port 3200


  Scenario: using a custom HTTP port
    When starting ExoCom at HTTP port 3200
    Then it opens an HTTP listener at port 3200


  Scenario: custom websocket port that is already taken
    Given another service already uses port 3200
    When I try starting ExoCom at websocket port 3200
    Then it aborts with the message "port 3200 is already in use"


  Scenario: custom HTTP port that is already taken
    Given another service already uses port 3200
    When I try starting ExoCom at HTTP port 3200
    Then it aborts with the message "port 3200 is already in use"
