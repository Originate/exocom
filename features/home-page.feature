Feature: Service Home Page

  As a devops person
  I want to be able to get a quick overview of a running service
  So that I know how it is doing without wasting further time logging into it.


  Rules:
  - the home page shows an HTML admin view of the service


  Scenario: viewing the homepage
    Given a running instance of the "hello-world" service
    When making a GET request to "/"
    Then the service shows "hello world"
