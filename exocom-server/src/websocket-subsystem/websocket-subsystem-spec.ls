require! {
  'chai' : {expect}
  '../../dist/exocom' : ExoCom
  'jsdiff-console'
  '../../features/support/mock-service' : MockService
  'record-http' : HttpRecorder
  'wait' : {wait, wait-until}
  'ws' : WebSocket
  './websocket-subsystem' : WebSocketSubsystem
}


describe 'WebSocket', ->


  describe '_internal-message-name', (...) ->

    before-each ->
      @exocom-websocket = new WebSocketSubsystem

    it 'translates the given message to the internal format of the given sender', ->
      service =
        name: 'tweets'
        internal-namespace: 'text-snippets'
      result = @exocom-websocket._internal-message-name 'tweets.create', for: service
      expect(result).to.eql 'text-snippets.create'


    it 'does not translate the given message if the recipient has the same internal namespace as the message', ->
      service =
        name: 'users'
        internal-namespace: 'users'
      result = @exocom-websocket._internal-message-name 'users.create', for: service
      expect(result).to.eql 'users.create'



    it 'does not translate the given message if it is not in a translatable format', ->
      result = @exocom-websocket._internal-message-name 'foo bar', for: {}
      expect(result).to.eql 'foo bar'
