require! {
  \wait : {wait, wait-until}
  \../support/websocket-endpoint : WebSocketEndpoint
}


World = !->

  @create-websocket-endpoint = (port, done) ->
    | @service  =>  return done!
    @service = new WebSocketEndpoint
      ..connect port, done


  @create-named-websocket-endpoint = ({name, exocom-port, registration-message, registration-payload}, done) ->
    @service = new WebSocketEndpoint name
      ..connect {exocom-port, registration-message, registration-payload}, ~>
          @exocom.wait-until-knows-service name, done


  @exocom-send-message = ({exocom, service, message-data}) ->
    exocom.send service: service, name: message-data.name, payload: message-data.payload



  @service-send-message = (message-data) ->
    @service.send message-data


  @verify-exocom-received-request = (expected-results) ->
    wait-until (~> @service.received-messages.length), 1, ~>
      actual-request = @service.received-messages[0]
      expect(actual-request.name).to.equal expected-request.NAME
      expect(actual-request.payload).to.equal expected-request.PAYLOAD



module.exports = ->
  @World = World
