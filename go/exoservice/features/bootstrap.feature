Feature: Bootstrap

  Rules:
  - use the "Bootstrap" method to
    - create an ExoRelay instance using environment variables
    - connects to ExoCom
    - registers the given handlers


  Scenario: replying to a message
    Given the "ping" test fixture runs
    When receiving a "ping" message with activityId "123"
    Then it sends a "pong" message as a reply to the message with activityId "123"
