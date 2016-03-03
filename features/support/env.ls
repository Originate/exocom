module.exports = ->

  @set-default-timeout 1000


  @After ->
    @existing-server?.close!
    @exocom?.close!
    @process?.kill!
    if @receivers
      for name, receiver of @receivers
        receiver.close!
