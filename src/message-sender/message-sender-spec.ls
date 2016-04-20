require! {
  'chai' : {expect}
  './message-sender' : MessageSender
  'jsdiff-console'
  'record-http' : HttpRecorder
  'wait' : {wait, wait-until}
}


describe 'MessageSender', ->

  before-each ->
    @message-sender = new MessageSender


  describe 'send-to-service', (...) ->

    before-each (done) ->
      @http-recorder = new HttpRecorder
        ..listen 4010
      wait 0, ~>
        message =
          name: 'message-1'
          payload: 'yo'
        service =
          name: 'users service'
          port: 4010
        @message-sender.send-to-service message, service, done

    after-each ->
      @http-recorder.close!

    it 'sends the given message to the given service', (done) ->
      condition = ~> @http-recorder.calls.length is 1
      wait-until condition, 10, ~>
        expected = [
          url: 'http://localhost:4010/run/message-1'
          method: 'POST'
          body:
            payload: 'yo'
          headers:
            accept: 'application/json'
            'content-type': 'application/json'
        ]
        jsdiff-console @http-recorder.calls, expected, done


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
