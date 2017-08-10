Feature: Linting Exosphere applications

  As an Exosphere developer
  I want to have an easy way to confirm all messages being sent are received somewhere
  So that I know that all my messages are handled.

  Rules:
  - run "exo lint" in the directory of your application to lint it
  - if a sent message is not received anywhere, it prints an error message
  - if a received message it not sent anywhere, it prints an error message

  Scenario: functioning application
    Given I am in the directory of an application with the services:
      | NAME  | LOCATION |
      | users | ./users  |
      | web   | ./web    |
    And my services are configured with:
      | LOCATION | SENDS                      | RECEIVES                   |
      | ./users  | user created, user deleted | create user, delete user   |
      | ./web    | create user, delete user   | user created, user deleted |
    When running "exocom-lint" in this application's directory
    Then the exit code is 0


  Scenario: some sent messages are not listened to
    Given I am in the directory of an application with the services:
      | NAME  | LOCATION |
      | users | ./users  |
      | web   | ./web    |
    And my services are configured with:
      | LOCATION | SENDS                      | RECEIVES                   |
      | ./users  | user created, user deleted | create user                |
      | ./web    | create user, delete user   | user created, user deleted |
    When running "exocom-lint" in this application's directory
    Then it prints:
      """
      The following messages are sent but not received:
        web: delete user
      """
    And the exit code is 1


  Scenario: some received messages are not sent
    Given I am in the directory of an application with the services:
      | NAME  | LOCATION |
      | users | ./users  |
      | web   | ./web    |
    And my services are configured with:
      | LOCATION | SENDS                      | RECEIVES                   |
      | ./users  | user deleted               | create user, delete user   |
      | ./web    | create user, delete user   | user created, user deleted |
    When running "exocom-lint" in this application's directory
    Then it prints:
      """
      The following messages are received but not sent:
        web: user created
      """
    And the exit code is 1
