require! {
  './client-registry' : ClientRegistry
  chai : {expect}
}


describe 'ClientRegistry', ->

  before-each ->
    @client-registry = new ClientRegistry


  describe 'external-message-name', (...) ->

    it "does not convert messages that don't match the format", ->
      service =
        name: 'tweets'
        internal-namespace: 'text-snippets'
      result = @client-registry.external-message-name 'foo bar', service
      expect(result).to.eql 'foo bar'

    it 'does not convert messages that have the same internal and external namespace', ->
      service =
        name: 'users'
        internal-namespace: 'users'
      result = @client-registry.external-message-name 'users.create', service
      expect(result).to.eql 'users.create'

    it 'does not convert messages if the service has no internal namespace', ->
      service =
        name: 'users'
        internal-namespace: ''
      result = @client-registry.external-message-name 'users.create', service
      expect(result).to.eql 'users.create'


    it 'converts messages into the external namespace of the service', ->
      service =
        name: 'tweets'
        internal-namespace: 'text-snippets'
      result = @client-registry.external-message-name 'text-snippets.create', service
      expect(result).to.eql 'tweets.create'
