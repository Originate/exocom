import { When } from 'cucumber'

When('ExoSocket connects to ExoCom', function() {
  thisexoComMockInstance = new MockExoCom()
  this.exoComMockInstance.listen(thos.exocomPort)
  this.exoSocketInstance.connect()
  this.exoSocketInstance.on('online', () => {

  })
    ..listen @exocom-port

  exoComMockInstance = newExocom(exocomPort)
  err := exoSocketInstance.Connect()
  if err != nil {
    return err
  }
  _, err = exoComMockInstance.WaitForConnection()
  return err
})
