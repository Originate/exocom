require! {
  'node-uuid' : uuid
  'rails-delegate' : {delegate}
  'record-http' : HttpRecorder
  'request'
}
debug = require('debug')('exocom-mock')


class MockExoCom


  ->
    @service-ports = {}
    @receiver = new HttpRecorder
      ..on 'receive', ~>
        debug "received incoming call"
        @receive-callback?!

    delegate \listen \port \reset \close, from: @, to: @receiver


  received-messages: ->
    [@_parse-call(call) for call in @receiver.calls]


  register-service: ({name, port}) ->
    debug "registering service '#{name}' at port #{port}"
    @service-ports[name] = port


  send-message: ({service, name, payload}) ->
    | !@service-ports[service]  =>  throw new Error "unknown service: '#{service}'"

    @reset!
    debug "sending message #{name} to service '#{service}'"
    request-data =
      url: "http://localhost:#{@service-ports[service]}/run/#{name}"
      method: 'POST'
      body:
        payload: payload
        id: uuid.v1!
      json: yes
    request request-data, (err, response) ~>
      if err
        return debug "error sending message '#{name}' to service '#{service}' at port #{@service-ports[service]}: #{err.message}"
      debug "received HTTP response #{response.status-code} from service '#{service}'"
      @last-send-response-code = response.status-code


  wait-until-receive: (@receive-callback) ->
    if @receiver.calls.length
      @receive-callback!


  _parse-call: (call) ->
    {
      name: @_get-message-name(call.url)
      payload: call.body.payload
    }



  _get-message-name: (url) ->
    segments = url.split '/'
    segments[segments.length-1]



module.exports = MockExoCom
