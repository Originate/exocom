Feature: Retrying

  Rules:
  - will retry connecting to ExoCom on initial connect and disconnections

  Background:
    Given an ExoSocket instance with the role "foo"

  Scenario: ExoRelay attempts to establish a connection to ExoCom
    Given ExoCom is offline
    When ExoSocket boots up a second before ExoCom
    Then ExoSocket should connect to ExoCom

  Scenario: ExoRelay will attempt to reconnect to ExoCom if ExoCom goes offline
    Given an ExoSocket instance that is connected to ExoCom
    When Exocom crashes and reboots
    Then ExoSocket should reconnect to ExoCom
