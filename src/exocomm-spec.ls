require! {
  '../dist/exocomm' : ExoComm
}


describe 'ExoComm', ->

  before-each ->
    @exocomm = new ExoComm


  describe 'EventEmitter', (...) ->

    it 'allows to register callbacks', ->
      @exocomm.on 'test', ->


    it 'allows to fire registered callbacks', (done) ->
      @exocomm.on 'test', done
      @exocomm.emit 'test'
