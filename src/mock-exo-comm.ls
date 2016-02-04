require! {
  'rails-delegate' : {delegate}
  'record-http' : HttpRecorder
  'request'
}


class MockExoComm


  ->
    @service-ports = {}
    @receiver = new HttpRecorder

    delegate \listen \port \reset \close, from: @, to: @receiver


  received-commands: ->
    [@_parse-call(call) for call in @receiver.calls]


  register-service: ({name, port}) ->
    @service-ports[name] = port


  send: ({service, command, payload}, done) ->
    | !@service-ports[service]  =>  return done new Error "unknown service: '#{service}'"

    request-data =
      url: "http://localhost:#{@service-ports[service]}/run/#{command}"
      method: 'POST'
      body:
        payload: payload
      json: yes
    request request-data, done



  _parse-call: (call) ->
    | call.method isnt 'POST'  =>  return

    {
      name: @_get-command-name(call.url)
      payload: call.body.payload
    }



  _get-command-name: (url) ->
    segments = url.split '/'
    segments[segments.length-1]


module.exports = MockExoComm
