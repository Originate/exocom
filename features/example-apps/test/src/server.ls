module.exports =

  before-all: (done) ->
    console.log 'Running before-all hook'
    done!


  'hello-world': ->
    console.log "Hello world!"


  'hello-name': (payload) ->
    console.log "Hello #{payload.name}!"


  ping: (_payload, {reply}) ->
    reply 'pong'


  sender: (_payload, {send}) ->
    send 'greetings', 'from the sender service'
