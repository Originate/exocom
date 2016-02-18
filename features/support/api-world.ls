require! {
  '../../dist/exocomm' : ExoComm
  './text-tools' : {ascii}
  'chai' : {expect}
  'jsdiff-console'
  'record-http' : HttpRecorder
  'wait' : {wait-until}
}


ApiWorld = !->

  @create-exocomm-instance = ({port}, done) ->
    @exocomm = new ExoComm
      ..listen port
      ..on 'listening', -> done!
      ..on 'message', (@last-broadcasted-message, @last-receivers) ~>


  @create-instance-at-port = (name, port, done) ->
    @receivers or= {}
    @receivers[name] = new HttpRecorder name
      ..listen port, done
      ..on 'receive', (@last-broadcasted-message, name) ~>


  @run-exocomm-at-port = (port, expect-error, done) ->
    @exocomm = new ExoComm
      ..listen port
    if expect-error
      @exocomm.on 'error', (@err) ~> done!
    else
      @exocomm.on 'listening', -> done!


  @service-sends-message = ({service, message, message-id = '123'} = {}, done) ->
    result = @exocomm.send-message name: message, request-id: message-id
    expect(result).to.equal 'success'
    done!


  @service-sends-reply = (service, reply-message, request-id, done) ->
    result = @exocomm.send-message name: reply-message, request-id: '123', response-to: request-id
    expect(result).to.equal 'success'
    done!


  @set-service-landscape = (service-data, done) ->
    result = @exocomm.set-services service-data
    expect(result).to.equal 'success'
    done!


  @verify-abort-with-message = (message, done) ->
    process.next-tick ~>
      expect(@err).to.equal message
      done!


  @verify-exocomm-broadcasted-message = ({message, services, response-to} done) ->
    wait-until (~> @last-broadcasted-message is message), 1, ~>
      expect(@last-receivers).to.eql services
      done!


  @verify-exocomm-broadcasted-reply = (message, done) ->
    wait-until (~> @last-broadcasted-message is message), 1, done


  @verify-routing-setup = (expected-routes, done) ->
    jsdiff-console @exocomm.get-config!routes, expected-routes, done


  @verify-exocomm-signals-broadcast = (message-name, done) ->
    @exocomm.on message-name, (message, receivers) ->
      console.log message
      done!


  @verify-runs-at-port = (port, done) ->
    expect(@exocomm.port).to.equal port
    done!


  @verify-sent-calls = ({service-name, message, request-id = '123', response-to}, done) ->
    service-receiver = @receivers[service-name]
    condition = -> service-receiver.calls.length is 1
    wait-until condition, 1, ~>
      expected = [
        url: "http://localhost:#{@ports[service-name]}/run/#{message}"
        method: 'POST'
        body:
          requestId: request-id
        headers:
          accept: 'application/json'
          'content-type': 'application/json'
      ]
      expected[0].body.response-to = response-to if response-to
      jsdiff-console service-receiver.calls, expected, done


  @verify-service-setup = (expected-services, done) ->
    jsdiff-console @exocomm.get-config!services, expected-services, done



module.exports = ->
  @World = ApiWorld if process.env.EXOCOMM_TEST_DEPTH is 'API'
