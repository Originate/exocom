Feature: Connecting

  Rules:
  - call "Connect" on an ExoRelay instance to take it online


  Background:
    Given an ExoSocket instance with the role "foo"


  Scenario: Setting up the ExoRelay instance
    When ExoSocket connects to ExoCom
    Then it registers by sending the message "exocom.register-service" with the sender "foo"
