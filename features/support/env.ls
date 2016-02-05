module.exports = ->

  @set-default-timeout 1500


  @After ->
    @server1?.close!
    @server2?.close!
    @process?.kill!
    @exocomm?.close!
