require! {
  './client-registry' : ClientRegistry
  'jsdiff-console'
}


describe 'ClientRegistry', ->

  before-each ->
    @client-registry = new ClientRegistry

  describe 'register', ->

    context 'no clients registered', (...) ->

      before-each ->
        @client-registry.register do
          name: 'test'
          sends: <[ command-1 command-2 ]>
          receives: <[ command-3 command-4 ]>
        @result = @client-registry.clients!


      it 'adds the given client to the registry', (done) ->
        expected = [
          name: 'test'
          sends: <[ command-1 command-2 ]>
          receives: <[ command-3 command-4 ]>
        ]
        jsdiff-console @result, expected, done


    context 'client is already registered', (...) ->

      before-each ->
        @client-registry.register do
          name: 'test'
          sends: <[ command-1 command-2 ]>
          receives: <[ command-3 command-4 ]>
        @client-registry.register do
          name: 'test'
          sends: <[ command-10 ]>
          receives: <[ command-11 ]>
        @result = @client-registry.clients!

      it 'overrides the given client', (done) ->
        expected = [
          name: 'test'
          sends: <[ command-10 ]>
          receives: <[ command-11 ]>
        ]
        jsdiff-console @result, expected, done
