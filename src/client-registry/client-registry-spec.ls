require! {
  './client-registry' : ClientRegistry
  chai : {expect}
}


describe 'ClientRegistry', ->

  before-each ->
    @client-registry = new ClientRegistry


  describe 'external-message-name', (...) ->

    it "does not convert messages that don't match the format", ->
      result = @client-registry.external-message-name do
        message: 'foo bar'
        service-name: 'tweets'
        internal-namespace: 'text-snippets'
      expect(result).to.eql 'foo bar'

    it 'does not convert messages that have the same internal and external namespace', ->
      result = @client-registry.external-message-name do
        message: 'users.create'
        service-name: 'users'
        internal-namespace: 'users'
      expect(result).to.eql 'users.create'

    it 'does not convert messages if the service has no internal namespace', ->
      result = @client-registry.external-message-name do
        message: 'users.create'
        service-name: 'users'
        internal-namespace: ''
      expect(result).to.eql 'users.create'


    it 'converts messages into the external namespace of the service', ->
      result = @client-registry.external-message-name do
        message: 'text-snippets.create'
        service-name: 'tweets'
        internal-namespace: 'text-snippets'
      expect(result).to.eql 'tweets.create'
