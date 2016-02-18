require! {
  '..' : MockExoComm
  'chai' : {expect}
  'jsdiff-console'
}


describe 'MockExoComm', ->

  before-each ->
    @exocomm = new MockExoComm!


  describe '_get-message-name', (...) ->

    it 'returns the message name part of the given URL', ->
      expect(@exocomm._get-message-name 'http://localhost:1234/send/users.create').to.equal 'users.create'


  describe '_parse-call', (...) ->

    it 'returns a data structure describing the parsed call', (done) ->
      call-data =
        url: 'http://localhost:1234/send/users.create'
        method: 'POST'
        body:
          payload: 'my payload'
        headers:
          accept: 'application/json'
      expected =
        name: 'users.create'
        payload: 'my payload'
      jsdiff-console @exocomm._parse-call(call-data), expected, done
