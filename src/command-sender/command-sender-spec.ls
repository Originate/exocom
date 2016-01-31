require! {
  './command-sender' : CommandSender
  'jsdiff-console'
  'record-http' : HttpRecorder
  'wait' : {wait, wait-until}
}


describe 'CommandSender', ->

  before-each ->
    @command-sender = new CommandSender


  describe 'send-to-service', (...) ->

    before-each (done) ->
      @http-recorder = new HttpRecorder
        ..listen 4010
      wait 0, ~>
        command =
          name: 'command-1'
          payload: 'yo'
        service =
          name: 'users service'
          port: 4010
        @command-sender.send-to-service command, service, done

    after-each ->
      @http-recorder.close!

    it 'sends the given command to the given service', (done) ->
      condition = ~> @http-recorder.calls.length is 1
      wait-until condition, 10, ~>
        expected = [
          url: 'http://localhost:4010/run/command-1'
          method: 'POST'
          body:
            payload: 'yo'
          headers:
            accept: 'application/json'
            'content-type': 'application/json'
        ]
        jsdiff-console @http-recorder.calls, expected, done
