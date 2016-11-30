module.exports =

  before-all: (done) ->
    console.log 'Running before-all hook'
    done!


  ping: (_, {reply}) ->
    reply 'pong'


  greet: (payload, {reply}) ->
    reply 'greeting', "Hello #{payload.name}"


  sender: (_payload, {send}) ->
    send 'greetings', 'from the sender service'
