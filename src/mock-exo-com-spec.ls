require! {
  './mock-exo-com.ls' : MockExoCom
  'chai' : {expect}
  'jsdiff-console'
}


describe 'MockExoCom', ->

  before-each ->
    @exocom = new MockExoCom!


  describe '_on-message', (...) ->

    it 'records a parsed WebSocket socket message', ->
      call =
        name: "users.create"
        payload: ""
      @exocom._on-message (JSON.stringify call |> JSON.parse)
      jsdiff-console @exocom.received-messages[0], call
