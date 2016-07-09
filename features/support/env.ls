module.exports = ->

  @set-default-timeout 1000


  @After ->
    @exo-relay?.close!
    @exo-relay?.close-port!
    @exocom?.close!


  @Before ->
    @ran = no
