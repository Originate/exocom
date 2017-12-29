import { After, Before, setWorldConstructor } from 'cucumber'
import World from './world'

const closeIfDefined = async obj => {
  if (obj) {
    await obj.close()
  }
}

After(async function() {
  await closeIfDefined(this.exoSocketInstance)
  await closeIfDefined(this.exoComMockInstance)
})

Before(function() {
  this.exocomPort = 4100
})

setWorldConstructor(World)
