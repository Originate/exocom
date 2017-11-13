import { Given } from 'cucumber'
import ExoSocket from '../../src/exo_socket'

Given('an ExoSocket instance with the role {string}', function(role) {
  this.exoSocketInstance = new ExoSocket({
    exocomHost: 'localhost',
    role,
  })
})
