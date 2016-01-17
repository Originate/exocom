module.exports = ->

  @set-default-timeout 1000


  @After ->
    @server1?.close!
    @server2?.close!
    @process?.kill!
