require! {
  'node-uuid' : uuid
  'rails-delegate' : {delegate}
  'record-http' : HttpRecorder
  'request'
}


class MockExoComm


  ->
    @service-ports = {}
    @receiver = new HttpRecorder
      ..on 'receive', ~> @receive-callback?!

    delegate \listen \port \reset \close, from: @, to: @receiver


  received-commands: ->
    [@_parse-call(call) for call in @receiver.calls]


  register-service: ({name, port}) ->
    @service-ports[name] = port


  send-command: ({service, name, payload}) ->
    | !@service-ports[service]  =>  throw new Error "unknown service: '#{service}'"

    request-data =
      url: "http://localhost:#{@service-ports[service]}/run/#{name}"
      method: 'POST'
      body:
        payload: payload
        request-id: uuid.v1!
      json: yes
    request request-data


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
