Feature: Configuring the ExoComm port

  As a developer
  I want to be able to configure the ExoComm port that my ExoRelay instance is talking to
  So that I have flexibility in my test setup.

  Rules:
  - the default ExoComm port is 3010
  - provide a custom ExoComm port via the "exocommPort" constructor parameter


  Scenario: the user does not provide the ExoComm port
    When I try to create an ExoRelay without providing the ExoComm port
    Then it throws the error "exocommPort not provided"


  Scenario: the user provides an available ExoComm port
    When I create an ExoRelay instance that uses ExoComm port 3011
    Then this instance uses the ExoComm port 3011
