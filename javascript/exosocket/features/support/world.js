import { expect } from 'chai'
import * as waitFor from './wait_for'

export default class World {
  async connectInstances() {
    this.exoComMockInstance.listen(this.exocomPort)
    await this.exoSocketInstance.connect()
    await new Promise(resolve => {
      this.exoComMockInstance.waitUntilKnowsService(this.role, resolve)
    })
  }

  waitForMessageCount(count, timeout) {
    return waitFor.condition(() => {
      expect(this.exoComMockInstance.receivedMessages).to.have.lengthOf(count)
    }, timeout)
  }

  async getTestFixtureReceivedMessage() {
    await waitFor.condition(() => {
      expect(this.testFixture.receivedMessages).to.have.lengthOf(1)
    }, 2000)
    return this.testFixture.receivedMessages[0]
  }

  async waitForConnection() {
    await this.waitForMessageCount(1, 2000)
    const message = this.exoComMockInstance.receivedMessages[0]
    expect(message.name).to.eql('exocom.register-service')
    expect(message.sender).to.eql(this.role)
  }
}
