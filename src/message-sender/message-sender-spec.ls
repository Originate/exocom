require! {
  'chai' : {expect}
  './message-sender' : MessageSender
  'jsdiff-console'
  'record-http' : HttpRecorder
  'wait' : {wait, wait-until}
  'zmq'
}


describe 'MessageSender', ->

  before-each ->
    @message-sender = new MessageSender


  describe 'send-to-service', (...) ->

    before-each (done) ->
      message =
          name: 'message-1'
          payload: 'payload-1'
          timestamp: 123
      services =
        'mock-service':
          name: 'mock-service'
          internal-namespace: 'mock-service'
          host: 'localhost'
          port: 3001
      @listener = zmq.socket 'pull'
        ..bind-sync "tcp://*:3001"
        ..on 'message', (data) ~> @data = JSON.parse data
      @message-sender
        ..clear-ports!
        ..bind-services services
      wait 0, ~>
        @message-sender.send-to-service message, services['mock-service'], done

    after-each ->
      @listener.close!
      @message-sender.clear-ports!

    it 'sends the given message to the given service', (done) ->
      wait-until (~> @data), 1, ~>
        expected =
          name: 'message-1'
          payload: 'payload-1'
          timestamp: 123
        jsdiff-console @data, expected, done


  describe '_translate', (...) ->

    it 'translates the given message to the internal format of the given sender', ->
      service =
        name: 'tweets'
        internal-namespace: 'text-snippets'
      result = @message-sender._translate 'tweets.create', for: service
      expect(result).to.eql 'text-snippets.create'


    it 'does not translate the given message if the recipient has the same internal namespace as the message', ->
      service =
        name: 'users'
        internal-namespace: 'users'
      result = @message-sender._translate 'users.create', for: service
      expect(result).to.eql 'users.create'



    it 'does not translate the given message if it is not in a translatable format', ->
      result = @message-sender._translate 'foo bar', for: {}
      expect(result).to.eql 'foo bar'
