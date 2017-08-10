Feature: Linting Exosphere applications with message translation (no external docker images)

  Scenario: functioning application
    Given I am in the directory of an application with the services:
      | NAME  | LOCATION |
      | web   | ./web    |
      | users | ./mongo  |
    And my services are configured with:
      | LOCATION | SENDS                          | RECEIVES                     |
      | ./web    | create user, delete user       | user created, user deleted   |
      | ./mongo  | record created, record deleted | create record, delete record |
    And my "users" service is configured with the message translation:
      | PUBLIC       | INTERNAL       |
      | create user  | create record  |
      | delete user  | delete record  |
      | user created | record created |
      | user deleted | record deleted |
    When running "exocom-lint" in this application's directory
    Then the exit code is 0


    Scenario: some sent messages are not listened to
      Given I am in the directory of an application with the services:
        | NAME  | LOCATION |
        | web   | ./web    |
        | users | ./mongo  |
      And my services are configured with:
        | LOCATION | SENDS                          | RECEIVES                     |
        | ./web    | create user, delete user       | user created                 |
        | ./mongo  | record created, record deleted | create record, delete record |
      And my "users" service is configured with the message translation:
        | PUBLIC       | INTERNAL       |
        | create user  | create record  |
        | delete user  | delete record  |
        | user created | record created |
        | user deleted | record deleted |
      When running "exocom-lint" in this application's directory
      Then it prints:
        """
        The following messages are sent but not received:
          users: user deleted
        """
      And the exit code is 1


  Scenario: some received messages are not sent
    Given I am in the directory of an application with the services:
      | NAME  | LOCATION |
      | web   | ./web    |
      | users | ./mongo  |
    And my services are configured with:
      | LOCATION | SENDS                          | RECEIVES                     |
      | ./web    | create user                    | user created, user deleted   |
      | ./mongo  | record created, record deleted | create record, delete record |
    And my "users" service is configured with the message translation:
      | PUBLIC       | INTERNAL       |
      | create user  | create record  |
      | delete user  | delete record  |
      | user created | record created |
      | user deleted | record deleted |
    When running "exocom-lint" in this application's directory
    Then it prints:
      """
      The following messages are received but not sent:
        users: delete user
      """
    And the exit code is 1


  Scenario: missing translation
    Given I am in the directory of an application with the services:
      | NAME  | LOCATION |
      | web   | ./web    |
      | users | ./mongo  |
    And my services are configured with:
      | LOCATION | SENDS                          | RECEIVES                     |
      | ./web    | create user, delete user       | user created, user deleted   |
      | ./mongo  | record created, record deleted | create record, delete record |
    And my "users" service is configured with the message translation:
      | PUBLIC       | INTERNAL       |
      | create user  | create record  |
      | user created | record created |
      | user deleted | record deleted |
    When running "exocom-lint" in this application's directory
    Then it prints:
      """
      The following messages are sent but not received:
        web: delete user
      The following messages are received but not sent:
        users: delete record
      """
    And the exit code is 1
