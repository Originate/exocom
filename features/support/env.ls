require! {
  'exosphere-shared' : {kill-child-processes}
}


module.exports = ->

  @set-default-timeout 1500


  @After (scenario, done) ->
    @server1?.close!
    @server2?.close!
    @process?.kill!
    @exocom?.close!
    @exoservice?.close!
    @exoservice?.close-port!
    if @app-dir
      fs.remove-sync @app-dir
    kill-child-processes done
