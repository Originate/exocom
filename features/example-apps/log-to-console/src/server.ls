module.exports =

  hello_world: (req) ->
    console.log "Hello world!"


  hello_name: (req) ->
    console.log "Hello #{req.body.name}!"
