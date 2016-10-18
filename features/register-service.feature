Feature: Register a service with ExoCom

  As an Exosphere operator
  I want new instances of my services to register themselves with ExoCom
  So that services can scale up and down automatically.

  Rules:
  - exoRelay registers itself with ExoCom when coming online via the ZMQ message "exocom.register-service"


  Background:
    Given ExoCom runs at port 4100

  Scenario: registering a service with ExoCom
    When an ExoRelay instance running inside the "test-service" service comes online at port 4000
    Then ExoRelay makes the ZMQ request:
      """
      name: 'exorelay.register'
      sender: 'test-service'
      payload:
        name: 'test-service'
        internal-namespace: undefined
        host: '<%= ip_address %>'
        port: 4000
      id: '<%= request_uuid %>'
      """
