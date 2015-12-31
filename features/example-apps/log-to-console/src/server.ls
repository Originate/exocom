module.exports =

  hello_world: (req, done) ->
    console.log "Hello world!"
    done!


  hello_name: (req, done) ->
    console.log "Hello #{req.body.name}!"
    done!


  forget_done: (req, done) ->
    console.log "forgetting to call done"


  double_done: (req, done) ->
    console.log "calling done twice"
    done!
    done!
