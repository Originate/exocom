import { After, Before, setWorldConstructor } from 'cucumber'
import World from './world'

After(async function() {
  if (this.exoSocketInstance) {
    await this.exoSocketInstance.close()
  }
  if (this.exoComMockInstance) {
    await new Promise(resolve => this.exoComMockInstance.close(resolve))
  }
})

Before(function() {
  this.exocomPort = 4100
})

setWorldConstructor(World)
