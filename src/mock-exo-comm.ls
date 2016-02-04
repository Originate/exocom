require! {
  'request'
}


class MockExoComm


  ->
    @service-ports = {}


  close: ->


  register-service: (name, port) ->
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



module.exports = MockExoComm
