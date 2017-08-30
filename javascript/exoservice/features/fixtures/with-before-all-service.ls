require! {
  '../..': {bootstrap}
}

hooks-ran = []


bootstrap do

  before-all: (done) ->
    hooks-ran.push 'before-all'
    done!


  'which-hooks-ran': (_, {reply}) ->
    reply 'these-hooks-ran', hooks-ran
