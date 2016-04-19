Feature: Broadcasting messages

  As an ExoService developer
  I want to be able to add a domain-specific API on top of generic off-the-shelf services
  So that I can quickly plug together applications without copy-and-pasting code around.

  Rules:
  - when a broadcasted message is delivered to a service,
    the service name is replaced with the service type in the message name
  - when a service broadcasts a message,
    ExoCom replaces the service type with the service name


  Background:
    Given a "web-server" instance running at port 3001
    And a "users-service" instance running at port 3002
    And an ExoCom instance configured for the service landscape:
      | NAME   | TYPE             | HOST      | PORT | SENDS            | RECEIVES        |
      | web    | web-server       | localhost | 3001 | tweets.create    | tweets.created  |
      | tweets | snippets-service | localhost | 3002 | snippets.created | snippets.create |


  Scenario: broadcasting a message
    When the web-server sends "tweets.create"
    Then ExoCom signals that this message is sent from the "web" service to the "tweets" service
    And ExoCom broadcasts the message "snippets.create" to the "tweets" service


  Scenario: broadcasting a reply
    Given the web-server sends "tweets.create" with id "111"
    When the users-service sends "snippets.created" in reply to "111"
    Then ExoCom signals that this reply is sent from the "tweets" to the "web-server" service
    And ExoCom broadcasts this reply to the web-server
