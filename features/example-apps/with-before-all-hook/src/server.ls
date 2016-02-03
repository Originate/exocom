module.exports =

  before-all: (done) ->
    console.log 'running beforeAll hook'
    done!


  hello: ->
