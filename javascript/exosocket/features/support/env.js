import { After, Before } from 'cucumber'

const closeIfDefined = (obj, done) => {
  if (obj) {
    obj.close(done)
  } else {
    done()
  }
}

After(function(_, done) {
  closeIfDefined(this.exoSocketInstance, () => {
    closeIfDefined(this.exoComMockInstance, done)
  })
})

Before(function() {
  this.exocomPort = 4100
})
