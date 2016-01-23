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


  Background:
    Given an ExoComm instance


  Scenario: a service registers itself with ExoComm
    When receiving a registration for this service:
      | NAME            | HOST      | PORT | SENDS     | RECEIVES  |
      | example service | localhost | 3001 | command 1 | command 2 |
    Then it knows about these services now:
      | NAME            | HOST      | PORT | SENDS     | RECEIVES  |
      | example service | localhost | 3001 | command 1 | command 2 |


  Scenario: a service updates its registration
    Given ExoComm knows about these services:
      | NAME            | HOST      | PORT | SENDS     | RECEIVES  |
      | example service | localhost | 3001 | command 1 | command 2 |
    When receiving a registration for this service:
      | NAME            | HOST      | PORT | SENDS     | RECEIVES  |
      | example service | localhost | 3002 | command 3 | command 4 |
    Then it knows about these services now:
      | NAME            | HOST      | PORT | SENDS     | RECEIVES  |
      | example service | localhost | 3002 | command 3 | command 4 |
