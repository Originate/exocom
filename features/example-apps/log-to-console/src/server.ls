module.exports =

  hello_world: ->
    console.log "Hello world!"


  hello_name: (req) ->
    console.log "Hello #{req.body.name}!"
