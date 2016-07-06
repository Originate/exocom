module.exports = ->

  @set-default-timeout 1000


  @After ->
    @exo-relay?.close!
    @exo-relay?.close-port!
    @exocom-listener?.close!
    @exocom-sender?.close!


  @Before ->
    @ran = no
