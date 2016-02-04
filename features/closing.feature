Feature: Closing the instance

  As a developer usin ExoCommMock in my tests
  I want to be able to shut it down clearly and completely
  So that I can set up a fresh instance for each test.

  Rules:
  - call "close" on your ExoCommMock instance to make it remove all its side effects


  Scenario: closing an active instance
    Given an ExoCommMock instance listening at port 4100
    When closing it
    Then it is no longer listening at port 4100


  Scenario: closing an inactive instance
    Given an ExoCommMock instance
    Then I can close it without errors
