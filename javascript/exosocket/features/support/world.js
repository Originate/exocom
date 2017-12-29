import { expect } from 'chai'
import * as waitFor from './wait_for'

export default class World {
  waitForMessageCount(count, timeout) {
    return waitFor.condition(() => {
      expect(this.exoComMockInstance.receivedMessages).to.have.lengthOf(count)
    }, timeout)
  }

  async waitForConnection() {
    await this.waitForMessageCount(1, 2000)
    const message = this.exoComMockInstance.receivedMessages[0]
    expect(message.name).to.eql('exocom.register-service')
    expect(message.sender).to.eql(this.role)
  }
}
