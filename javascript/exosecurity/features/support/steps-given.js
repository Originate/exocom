const { Given } = require('cucumber')
const ExoComMock = require('../../../exocom-mock')
const N = require('nitroglycerin')
const portReservation = require('port-reservation')

Given(/^an ExoCom instance$/, function(done) {
  portReservation.getPort(
    N(exocomPort => {
      this.exocomPort = exocomPort
      this.exocom = new ExoComMock()
      this.exocom.listen(this.exocomPort)
      done()
    })
  )
})

Given(/^receiving this message:$/, function(messageStr) {
  const message = JSON.parse(messageStr)
  this.exocom.reset()
  this.exocom.send(this.role, message)
})

Given(/^an instance of the "([^"]*)" fixture$/, function(role, done) {
  this.role = role
  this.createExosecurityInstance(
    {
      role: this.role,
      exocomPort: this.exocomPort,
    },
    () => {
      this.removeRegisterServiceMessage(this.exocom, done)
    }
  )
})
