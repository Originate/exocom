Feature: status check

  As an administrator
  I want to have an easy way to check whether an ExoRelay is online
  So that I know whether my system is set up correctly.

  Rules:
  - make a GET request to "/status" to check whether that ExoRelay is online
  - if it is, it returns a 200 response
  - any other return code or non-response means an error


  Scenario: ExoRelay is online
    Given ExoCom runs at port 4100
    And an ExoRelay instance listening at port 4000
    When I check the status
    Then it signals it is online
