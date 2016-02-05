require! {
  'node-uuid' : uuid
  'rails-delegate' : {delegate}
  'record-http' : HttpRecorder
  'request'
}
debug = require('debug')('exocomm-mock')


class MockExoComm


  ->
    @service-ports = {}
    @receiver = new HttpRecorder
      ..on 'receive', ~>
        debug "received incoming call"
        @receive-callback?!

    delegate \listen \port \reset \close, from: @, to: @receiver


  received-commands: ->
    [@_parse-call(call) for call in @receiver.calls]


  register-service: ({name, port}) ->
    debug "registering service #{name} at port #{port}"
    @service-ports[name] = port


  send-command: ({service, name, payload}) ->
    | !@service-ports[service]  =>  throw new Error "unknown service: '#{service}'"

    debug "sending command #{name} to service #{service}"
    request-data =
      url: "http://localhost:#{@service-ports[service]}/run/#{name}"
      method: 'POST'
      body:
        payload: payload
        request-id: uuid.v1!
      json: yes
    request request-data, (err, response) ~>
      if err
        return debug "error sending command '#{name}' to service '#{service}' at port #{@service-ports[service]}: #{err.message}"
      debug "received HTTP response #{response.status-code}"
      @last-send-response-code = response.status-code


  wait-until-receive: (@receive-callback) ->
    if @receiver.calls.length
      @receive-callback!


  _parse-call: (call) ->
    {
      name: @_get-command-name(call.url)
      payload: call.body.payload
    }



  _get-command-name: (url) ->
    segments = url.split '/'
    segments[segments.length-1]


module.exports = MockExoComm
