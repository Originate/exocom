module.exports = ->

  @set-default-timeout 1000


  @After ->
    @exo-relay?.close!
    @exocomm?.close!


  @Before ->
    @ran = no
