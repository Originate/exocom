Feature: Translating messages

  As an ExoService developer
  I want to be able to use generic off-the-shelf services with a domain-specific API
  So that I can quickly plug together application prototypes without copy-and-pasting code around.

  - when a broadcasted message is delivered to a service,
    the service name in the message name is replaced with the service type
  - when a service broadcasts a message,
    Exocom replaces the service type with the external service name


  Background:
    Given an ExoCom instance with routing information "[{name: web, receives: [tweets.created]}, {name: tweets, receives: [text-snippet.create]}]" configured for the service landscape:
      | NAME   | INTERNAL NAMESPACE | HOST      | PORT |
      | web    |                    | localhost | 3001 |
      | tweets | text-snippets      | localhost | 3002 |
    And a "web" instance running at port 3001
    And a "tweets" instance running at port 3002


  Scenario: translating a message
    When the "web" service sends "tweets.create"
    Then ExoCom signals "web  --[ tweets.create ]-[ text-snippets.create ]->  tweets"
    And ExoCom broadcasts the message "text-snippets.create" to the "tweets" service


  Scenario: translating a reply
    When the "web" service sends "tweets.create"
    And the "tweets" service sends "text-snippets.created" in reply to "111"
    Then ExoCom signals "tweets  --[ text-snippets.created ]-[ tweets.created ]->  web  (XX ms)"
    And ExoCom broadcasts the reply "tweets.created" to the "web" service
