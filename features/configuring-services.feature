Feature: Configuring services

  As an ExoSphere operator
  I want be able to tell ExoCom my current service setup
  So that it can send them messages.

  - only the Exosphere environment can tell ExoCom about the current service layout
  - this is done via a POST request to "/config"


  Background:
    Given an ExoCom instance


  Scenario: setting the service configuration
    When sending the service configuration:
      """
      * name: 'service 1'
        internal-namespace: 'foo'
        host: 'localhost'
        port: 3001
        sends: ['message-1']
        receives: ['message-2']
      * name: 'service 2'
        internal-namespace: 'bar'
        host: 'localhost'
        port: 3002
        sends: ['message-2']
        receives: ['message-1']
      """
    Then ExoCom now knows about these services:
      | NAME      | INTERNAL NAMESPACE | HOST      | PORT |
      | service 1 | foo                | localhost | 3001 |
      | service 2 | bar                | localhost | 3002 |
    And it has this routing table:
      | MESSAGE   | SENDERS   | RECEIVERS                                                                     |
      | message-1 | service 1 | {name: 'service 2', internal-namespace: 'bar', host: 'localhost', port: 3002} |
      | message-2 | service 2 | {name: 'service 1', internal-namespace: 'foo', host: 'localhost', port: 3001} |
