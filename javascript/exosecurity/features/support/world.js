const ObservableProcess = require('observable-process')
const path = require('path')
const { some } = require('lodash')
const { waitUntil } = require('wait')

const observableProcessOptions = process.env.DEBUG_EXOSERVICE
  ? {
      verbose: true,
      stdout: process.stdout,
      stderr: process.stderr,
    }
  : {
      verbose: false,
      stdout: false,
      stderr: false,
    }

function World() {
  this.createExosecurityInstance = function({ role, exocomPort }, done) {
    const fixturePath = path.join(process.cwd(), 'features', 'fixtures', role)
    this.process = new ObservableProcess(`node ${fixturePath}`, {
      env: {
        EXOCOM_PORT: exocomPort,
        ROLE: role,
      },
      verbose: observableProcessOptions.verbose,
      stdout: observableProcessOptions.stdout,
      stderr: observableProcessOptions.stderr,
    })
    this.process.wait('online at port', done)
  }
  this.removeRegisterServiceMessage = function(exocom, done) {
    waitUntil(() => this.exocom.receivedMessages.length, 10, () => {
      if (
        some(this.exocom.receivedMessages, ['name', 'exocom.register-service'])
      ) {
        this.exocom.reset()
      }
      done()
    })
  }
}
module.exports = World
