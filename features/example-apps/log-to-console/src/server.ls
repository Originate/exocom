module.exports =

  'hello-world': ->
    console.log "Hello world!"


  'hello-name': (payload) ->
    console.log "Hello #{payload.name}!"

  'ping': (_payload, {reply}) ->
    reply 'pong'
