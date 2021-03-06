Feature:

  As an Exosphere developer
  I want to be able to send messages from my Exosphere application to a client
  So that I can create a useful application

  Rules
  - messages received from exorelay are forwarded to a client only if the auth matches


  Background:
    Given a frontend bridge connected to Exocom


  Scenario: receiving a message with a auth
    When receiving this message from the client:
      """
      {
        "name": "ping",
        "id": "123"
      }
      """
    And receiving this message from exocom:
      """
      {
        "name": "pong",
        "auth": "{{previous message auth}}",
        "id": "456"
      }
      """
    Then frontend bridge sends this to the client:
      """
      {
        "name": "pong",
        "id": "456"
      }
      """


  Scenario: receiving a message with no auth
    When receiving this message from the client:
      """
      {
        "name": "ping",
        "id": "123"
      }
      """
    And receiving this message from exocom:
      """
      {
        "name": "pong",
        "id": "456"
      }
      """
    Then the frontend bridge does not send a message to the client
