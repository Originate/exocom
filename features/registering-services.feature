Feature: Registering services

  As an ExoSphere operator
  I want ExoComm to learn about client services
  So that it can send them commands

  Rules:
  - services can register with ExoComm by sending an "exosphere.register-service" command to it
  - the payload for this command contains:
    - the name of the service
    - the IP address and port of the service
    - the list of commands this service wants to send and receive


  Scenario: a service registers itself with ExoComm
    Given an ExoComm instance at port 4100
    When receiving a service registration via this request:
      """
      url: "http://localhost:4100/register-service",
      method: 'POST'
      body:
        payload:
          name: 'example service'
          host: 'localhost'
          port: 3001
          sends:
            * 'command 1'
            * 'command 2'
          receives:
            * 'command 3'
            * 'command 4'
      """
    Then it knows about these services:
      """
      * name: 'example service'
        host: 'localhost'
        port: 3001
        sends:
          * 'command 1'
          * 'command 2'
        receives:
          * 'command 3'
          * 'command 4'
      """


  Scenario: a service updates its registration
    Given an ExoComm instance at port 4100
    And it knows about this service:
      """
      name: 'example service'
      host: 'localhost'
      port: 3001
      sends:
        * 'command 1'
      receives:
        * 'command 2'
      """
    When receiving a service registration via this request:
      """
      url: "http://localhost:4100/register-service",
      method: 'POST'
      body:
        payload:
          name: 'example service'
          host: 'localhost'
          port: 3002
          sends:
            * 'command 6'
          receives:
            * 'command 7'
        requestId: '123'
      """
    Then it knows about these services:
      """
      * name: 'example service'
        host: 'localhost'
        port: 3002
        sends:
          * 'command 6'
        receives:
          * 'command 7'
      """
