require! {
  './client-registry' : ClientRegistry
  'jsdiff-console'
  'nitroglycerin' : N
}


describe 'ClientRegistry', ->

  before-each ->
    @client-registry = new ClientRegistry
    @client-registry.register do
      name: 'service 1'
      sends: ['command-1']
      receives: ['command-2']
    @client-registry.register do
      name: 'service 2'
      sends: ['command-1']
      receives: ['command-2']


  describe 'client', ->

    context 'known client', (...) ->

      it 'returns the data for the given client', (done) ->
        expected =
          name: 'service 1'
          sends: ['command-1']
          receives: ['command-2']
        jsdiff-console @client-registry.client('service 1'), expected, done


  describe 'clients', (...) ->

    it 'returns the list of registered clients', (done) ->
      expected =
        * name: 'service 1'
          sends: ['command-1']
          receives: ['command-2']
        * name: 'service 2'
          sends: ['command-1']
          receives: ['command-2']
      jsdiff-console @client-registry.clients!, expected, done


  describe 'register', ->

    context 'client is already registered', (...) ->

      before-each ->
        @client-registry.register do
          name: 'service 1'
          sends: ['command-10']
          receives: ['command-11']

      it 'overrides the given client', (done) ->
        expected =
          * name: 'service 2'
            sends: ['command-1']
            receives: ['command-2']
          * name: 'service 1'
            sends: ['command-10']
            receives: ['command-11']
        jsdiff-console @client-registry.clients!, expected, done


  describe 'remove-service', (...) ->

    before-each ->
      @client-registry.remove-service 'service 1'

    it 'removes the given service from the client list', (done) ->
      expected = [
        name: 'service 2'
        sends: ['command-1']
        receives: ['command-2']
      ]
      jsdiff-console @client-registry.clients!, expected, done

    it 'removes the given service from all subscriber lists', (done) ->
      jsdiff-console @client-registry.subscriber-names-to('command-2'), ['service 2'], done


  describe 'subscriber-names-to', (...) ->

    it 'returns the names of services that are subscribed to the command with the given name', (done) ->
      jsdiff-console @client-registry.subscriber-names-to('command-2'),
                     ['service 1', 'service 2'],
                     done

    it 'returns an empty array if there are no subscribers', (done) ->
      jsdiff-console @client-registry.subscriber-names-to('command-X'),
                     [],
                     done

  describe 'subscribers-to', (...) ->

    it 'returns the complete client data of subscribers to the command with the given name', (done) ->
      expected =
        * name: 'service 1'
          sends: ['command-1']
          receives: ['command-2']
        * name: 'service 2'
          sends: ['command-1']
          receives: ['command-2']
      jsdiff-console @client-registry.subscribers-to('command-2'), expected, done
