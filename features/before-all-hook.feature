Feature: Before-All hook

  As a developer using external resources that have to be initialized before going online
  I want to be able to signal to ExoService when I am done initializing
  So that the service doesn't signal availability before everything is properly initialized.

  Rules:
  - put initializiation code into a "beforeAll" command handler


  Scenario: code with beforeAll hook
    Given I am in the "with-before-all-hook" service directory
    When executing "exo-js run"
    Then its console output contains "running beforeAll hook"
    And its console output contains "online at port"


  Scenario: code without a beforeAll hook
    Given I am in the "without-before-all-hook" service directory
    When executing "exo-js run"
    Then its console output contains "online at port"
