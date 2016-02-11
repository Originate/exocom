Feature: Configuring services

  As an ExoSphere operator
  I want be able to tell ExoComm my current service setup
  So that it can send them commands.

  Rules:
  - only the Exosphere environment can tell ExoComm about the current service layout
  - this is done via a POST request to "/config"


  Background:
    Given an ExoComm instance


  Scenario: setting the service configuration
    When setting this service landscape:
      | NAME      | HOST      | PORT | SENDS     | RECEIVES  |
      | service 1 | localhost | 3001 | command-1 | command-2 |
      | service 2 | localhost | 3002 | command-2 | command-1 |
    Then ExoComm now knows about these services:
      | NAME      | HOST      | PORT |
      | service 1 | localhost | 3001 |
      | service 2 | localhost | 3002 |
    And it has this routing table:
      | COMMAND   | SENDERS   | RECEIVERS                                          |
      | command-1 | service 1 | {name: 'service 2', host: 'localhost', port: 3002} |
      | command-2 | service 2 | {name: 'service 1', host: 'localhost', port: 3001} |
