const {
  After,
  Before,
  setDefaultTimeout,
  setWorldConstructor,
} = require('cucumber')
const { killChildProcesses } = require('exosphere-shared')
const World = require('./world')

setDefaultTimeout(1000)
setWorldConstructor(World)

function closeIfDefined(obj, done) {
  if (obj) {
    obj.close(done)
  } else {
    done()
  }
}

After(function(scenario, done) {
  if (this.process != null) {
    this.process.kill()
  }
  closeIfDefined(this.exosecurity, () =>
    closeIfDefined(this.exocom, () => killChildProcesses(done))
  )
})
Before(function() {
  this.ran = false
})
