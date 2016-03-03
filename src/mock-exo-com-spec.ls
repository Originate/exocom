require! {
  '..' : MockExoCom
  'chai' : {expect}
  'jsdiff-console'
}


describe 'MockExoCom', ->

  before-each ->
    @exocom = new MockExoCom!


  describe '_get-message-name', (...) ->

    it 'returns the message name part of the given URL', ->
      expect(@exocom._get-message-name 'http://localhost:1234/send/users.create').to.equal 'users.create'


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
      jsdiff-console @exocom._parse-call(call-data), expected, done
