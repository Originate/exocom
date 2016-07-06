require! {
  './message-sender' : MessageSender
  'chai' : {expect}
  'sinon'
}

describe 'MessageSender', ->

  before-each ->
    @message-sender = new MessageSender exocom-port: 4100, service-name: 'test'
      ..on 'error', (@error) ~>

  after-each ->
    @message-sender.close-port!


  describe 'reply-method-for', (...) ->

    before-each ->
      @message-sender.send = sinon.stub!
      @reply-method = @message-sender.reply-method-for '123'

    it 'returns a function', ->
      expect(typeof @reply-method).to.equal 'function'

    it 'calls @send', ->
      @reply-method!
      expect(@message-sender.send.called).to.be.true

    it 'pre-populates the id', ->
      @reply-method 'reply-message', 'payload'
      expect(@message-sender.send.first-call.args).to.eql [ 'reply-message', 'payload', response-to: '123' ]


    context 'missing id', (...) ->

      before-each ->
        @message-sender.reply-method-for null

      it 'emits an error', (done) ->
        expect(@error.message).to.eql 'MessageSender.replyMethodFor needs a id'
        done!
