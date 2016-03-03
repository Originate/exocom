module.exports = ->

  @set-default-timeout 1000


  @After ->
    @exocom?.close!
