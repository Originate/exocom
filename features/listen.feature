Feature: Listening

  As an Exosphere user
  I want to be able to add Exosphere communication to my Node application
  So that I can write Exosphere applications using Node.js


  Rules
  - create an ExoRelay instance to boot this library
  - call "listen" on that instance to bring it online at the given port


  Scenario: Setting up on a free port
    Given a running "hello-world" example application
    When making a POST request to "http://localhost:4000/run/hello"
    Then it returns a 200 response
