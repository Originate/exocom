module.exports = ->

  @After ->
    @process?.kill!
