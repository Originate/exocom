require! {
  './command-sender' : CommandSender
  'chai' : {expect}
  'sinon'
}

describe 'CommandSender', ->

  before-each ->
    @command-sender = new CommandSender!
      ..on 'error', (@error) ~>


  describe 'reply-method-for', (...) ->

    before-each ->
      @command-sender.send = sinon.stub!
      @reply-method = @command-sender.reply-method-for '123'

    it 'returns a function', ->
      expect(typeof @reply-method).to.equal 'function'

    it 'calls @send', ->
      @reply-method!
      expect(@command-sender.send.called).to.be.true

    it 'pre-populates the request-id', ->
      @reply-method 'reply-command', 'payload'
      expect(@command-sender.send.first-call.args).to.eql [ 'reply-command', 'payload', response-to: '123' ]


    context 'missing request-id', (...) ->

      before-each ->
        @command-sender.reply-method-for null

      it 'emits an error', (done) ->
        expect(@error.message).to.eql 'CommandSender.replyMethodFor needs a requestId'
        done!
