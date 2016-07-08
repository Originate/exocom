require! {
  '..' : MockExoCom
  'chai' : {expect}
  'jsdiff-console'
}


describe 'MockExoCom', ->

  before-each ->
    @exocom = new MockExoCom!


  describe '_on-pull-socket-message', (...) ->

    it 'records a parsed ZMQ socket message', ->
      call =
        name: "users.create"
        payload: ""
      @exocom._on-pull-socket-message JSON.stringify call
      jsdiff-console @exocom.received-messages[0], call
