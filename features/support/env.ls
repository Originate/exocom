module.exports = ->

  @set-default-timeout 1000


  @After ->
    @existing-server?.close!
    @exocom?.close!
    @exocom?.clearPorts!
    @process?.kill!
    for name, mock of @service-mocks or {}
      mock.close!
