require! {
  '../../dist/exocom' : ExoCom
  './text-tools' : {ascii}
  'chai' : {expect}
  'jsdiff-console'
  'record-http' : HttpRecorder
  'wait' : {wait-until}
}


ApiWorld = !->

  @create-exocom-instance = ({port}, done) ->
    @exocom = new ExoCom
      ..listen port
      ..on 'listening', -> done!
      ..on 'message', ({sender, message, receivers}) ~>
        @last-sender = sender
        @last-broadcasted-message = message
        @last-receivers = receivers


  @create-instance-at-port = (name, port, done) ->
    @receivers or= {}
    @receivers[name] = new HttpRecorder name
      ..listen port, done
      ..on 'receive', (@last-broadcasted-message, name) ~>


  @run-exocom-at-port = (port, expect-error, done) ->
    @exocom = new ExoCom
      ..listen port
    if expect-error
      @exocom.on 'error', (@err) ~> done!
    else
      @exocom.on 'listening', -> done!


  @service-sends-message = ({service, message, id = '123'} = {}, done) ->
    result = @exocom.send-message name: message, sender: service, id: id
    expect(result).to.equal 'success'
    done!


  @service-sends-reply = ({service, message, response-to}, done) ->
    result = @exocom.send-message name: message, sender: service, id: '123', response-to: response-to
    expect(result).to.equal 'success'
    done!


  @set-service-landscape = (service-data, done) ->
    result = @exocom.set-services service-data
    expect(result).to.equal 'success'
    done!


  @verify-abort-with-message = (message, done) ->
    process.next-tick ~>
      expect(@err).to.equal message
      done!


  @verify-exocom-broadcasted-message = ({sender, message, receivers, response-to} done) ->
    wait-until (~> @last-broadcasted-message is message), 1, ~>
      expect(@last-sender).to.eql sender
      expect(@last-receivers).to.eql receivers
      done!


  @verify-exocom-broadcasted-reply = (message, done) ->
    wait-until (~> @last-broadcasted-message is message), 1, done


  @verify-routing-setup = (expected-routes, done) ->
    jsdiff-console @exocom.get-config!routes, expected-routes, done


  @verify-exocom-signals-broadcast = (message-name, done) ->
    @exocom.on message-name, (message, receivers) ->
      console.log message
      done!


  @verify-runs-at-port = (port, done) ->
    expect(@exocom.port).to.equal port
    done!


  @verify-sent-calls = ({service-name, message, id = '123', response-to}, done) ->
    service-receiver = @receivers[service-name]
    condition = -> service-receiver.calls.length is 1
    wait-until condition, 1, ~>
      expected = [
        url: "http://localhost:#{@ports[service-name]}/run/#{message}"
        method: 'POST'
        body:
          id: id
        headers:
          accept: 'application/json'
          'content-type': 'application/json'
      ]
      expected[0].body.response-to = response-to if response-to
      jsdiff-console service-receiver.calls, expected, done


  @verify-service-setup = (expected-services, done) ->
    jsdiff-console @exocom.get-config!services, expected-services, done



module.exports = ->
  @World = ApiWorld if process.env.EXOCOM_TEST_DEPTH is 'API'
