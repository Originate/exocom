Feature: Listening

  As a developer writing Exosphere applications
  I want to be able to talk to Exosphere from my Node.js application
  So that I can write Exosphere applications using Node.js.

  Rules
  - create an ExoRelay instance to boot this library
  - register handlers for incoming commands using "registerHandler"
  - call "listen" on that instance to bring it online at the given port


  Scenario: Setting up on a free port
    Given a running "hello-world" example application
    When sending a POST request to "http://localhost:4000/run/hello"
    Then it returns a 200 response
