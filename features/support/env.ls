module.exports = ->

  @set-default-timeout 1000


  @After ->
    @exocomm?.close!
