require! {
  '../..' : ExoRelay
}


module.exports =

  'hello-world': (done) ->
    new ExoRelay!
      ..register-handler 'hello', -> console.log 'hello world!'
      ..listen 4000, done
