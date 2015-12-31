module.exports =

  hello: (req, done) ->
    console.log "Hello there!"
    done!


  forget_done: (req, done) ->
    console.log "forgetting to call done"


  double_done: (req, done) ->
    console.log "calling done twice"
    done!
    done!
