Feature: Waiting until a command is received

  As a developer expecting my service under test to send out a command
  I want to have a convenient API to wait for that to happen
  So that I don't have to set up boilerplate in my tests to do this myself.

  Rules:
  - call "exocomm.waitUntilReceive" to wait until ExoComm has received an incoming command


  Background:
    Given a listening ExoCommMock instance


  Scenario: a call is about to be received
    When I tell it to wait for a call
    Then it doesn't call the given callback right away
    When a call comes in
    Then it calls the given callback


  Scenario: a call has already been received
    Given a call comes in
    When I tell it to wait for a call
    Then it calls the given callback right away
