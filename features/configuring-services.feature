Feature: Configuring services

  As an ExoSphere operator
  I want be able to tell ExoCom my current service setup
  So that it can send them messages.

  Rules:
  - only the Exosphere environment can tell ExoCom about the current service layout
  - this is done via a POST request to "/config"


  Background:
    Given an ExoCom instance


  Scenario: setting the service configuration
    When setting this service landscape:
      | NAME      | HOST      | PORT | SENDS     | RECEIVES  |
      | service 1 | localhost | 3001 | message-1 | message-2 |
      | service 2 | localhost | 3002 | message-2 | message-1 |
    Then ExoCom now knows about these services:
      | NAME      | HOST      | PORT |
      | service 1 | localhost | 3001 |
      | service 2 | localhost | 3002 |
    And it has this routing table:
      | MESSAGE   | SENDERS   | RECEIVERS                                          |
      | message-1 | service 1 | {name: 'service 2', host: 'localhost', port: 3002} |
      | message-2 | service 2 | {name: 'service 1', host: 'localhost', port: 3001} |
