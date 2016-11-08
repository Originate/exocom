Feature: Listening

  As a developer building Exosphere applications
  I want to be able to add an Exosphere communication relay to any code base
  So that I can write Exosphere services without constraints on my code layout.

  Rules
  - call "connect" on an ExoRelay instance to take it online


  Background:
    Given ExoCom runs at port 4100
    And a new ExoRelay instance


  Scenario: Setting up the ExoRelay instance
    When I take it online
    Then it connects to the given ExoCom host and port
