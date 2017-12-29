import { Given } from 'cucumber'
import ExoSocket from '../../src/exo_socket'
import testFixtures from '../../test_fixtures'
import MockExoCom from '../../../exocom-mock'

Given('an ExoSocket instance with the role {string}', function(role) {
  this.role = role
  this.exoSocketInstance = new ExoSocket({
    exocomPort: this.exocomPort,
    exocomHost: 'localhost',
    role,
  })
  this.exoSocketInstance.on('error', e => {
    this.error = e
  })
})

Given('ExoCom is offline', () => {
  // noop
})

Given('I setup the {string} test fixture', function(name) {
  this.testFixture = new testFixtures[name]()
  this.testFixture.setup(this.exoSocketInstance)
})

Given('an ExoSocket instance that is connected to ExoCom', function() {
  this.exoComMockInstance = new MockExoCom()
  return this.connectInstances()
})
