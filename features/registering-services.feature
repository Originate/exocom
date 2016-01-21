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
    When receiving a service registration via this command:
      """
      url: "http://localhost:4100/register-service",
      method: 'POST'
      body:
        payload:
          name: 'example service'
          sends:
            * 'send command 1'
            * 'send command 2'
          receives:
            * 'receive command 1'
            * 'receive command 2'
      """
    Then it knows about these services:
      """
      * name: 'example service'
        sends:
          * 'send command 1'
          * 'send command 2'
        receives:
          * 'receive command 1'
          * 'receive command 2'
      """


  Scenario: a service updates its registration
    Given an ExoComm instance at port 4100
    And it knows about this service:
      """
      name: 'example service'
      sends:
        * 'send command 1'
        * 'send command 2'
      receives:
        * 'receive command 1'
        * 'receive command 2'
      """
    When receiving a service registration via this command:
      """
      url: "http://localhost:4100/register-service",
      method: 'POST'
      body:
        payload:
          name: 'example service'
          sends:
            * 'send command 3'
          receives:
            * 'receive command 3'
        requestId: '123'
      """
    Then it knows about these services:
      """
      * name: 'example service'
        sends:
          * 'send command 3'
        receives:
          * 'receive command 3'
      """
