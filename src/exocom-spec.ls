require! {
  '../dist/exocom' : ExoCom
}


describe 'ExoCom', ->

  before-each ->
    @exocom = new ExoCom


  describe 'EventEmitter', (...) ->

    it 'allows to register callbacks', ->
      @exocom.on 'test', ->


    it 'allows to fire registered callbacks', (done) ->
      @exocom.on 'test', done
      @exocom.emit 'test'
