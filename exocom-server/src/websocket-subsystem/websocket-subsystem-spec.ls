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


  describe 'send-message-to-service', (...) ->

    before-each (done) ->
      message =
          name: 'message-1'
          payload: 'payload-1'
      services =
        'mock-service':
          name: 'mock-service'
          internal-namespace: 'mock-service'
      @exocom = new ExoCom service-messages: "[{name: mock-service, receives: [message-1]}]"
        ..listen 3001
        ..on 'websockets-online', ~>
          @mock-service = new MockService {port: 3001, name: 'mock-service', namespace: 'mock-service'}
            ..connect {}, ~>
              wait 200, ~>
                @exocom.websocket
                  ..send-message-to-service message, services['mock-service']
                done!

    after-each ->
      @exocom.close!

    it 'sends the given message to the given service', (done) ->
      wait-until (~> @mock-service.received-messages.length), 1, ~>
        expected =
          name: 'message-1'
          payload: 'payload-1'
        jsdiff-console @mock-service.received-messages[0], expected, done


  describe '_translate', (...) ->

    before-each ->
      @exocom-websocket = new WebSocketSubsystem

    it 'translates the given message to the internal format of the given sender', ->
      service =
        name: 'tweets'
        internal-namespace: 'text-snippets'
      result = @exocom-websocket._translate 'tweets.create', for: service
      expect(result).to.eql 'text-snippets.create'


    it 'does not translate the given message if the recipient has the same internal namespace as the message', ->
      service =
        name: 'users'
        internal-namespace: 'users'
      result = @exocom-websocket._translate 'users.create', for: service
      expect(result).to.eql 'users.create'



    it 'does not translate the given message if it is not in a translatable format', ->
      result = @exocom-websocket._translate 'foo bar', for: {}
      expect(result).to.eql 'foo bar'
