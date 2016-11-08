require! {
  \wait : {wait, wait-until}
  \../support/websocket-endpoint : WebSocketEndpoint
}


World = !->

  @create-websocket-endpoint = (port) ->
    unless @service
      @service = new WebSocketEndpoint
        ..listen port


  @create-named-websocket-endpoint = ({name, port}) ->
    @service = new WebSocketEndpoint name
      ..listen port


  @exocom-send-message = ({exocom, service, message-data}, done) ->
    wait-until (~> @service.can-send), 10, ~>
      exocom.send service: service, name: message-data.name, payload: message-data.payload
      done!



  @service-send-message = (message-data, done) ->
    wait-until (~> @service.can-send), 10, ~>
      @service.send message-data
      done!


  @verify-exocom-received-request = (expected-results) ->
    wait-until (~> @service.received-messages.length), 1, ~>
      actual-request = @service.received-messages[0]
      expect(actual-request.name).to.equal expected-request.NAME
      expect(actual-request.payload).to.equal expected-request.PAYLOAD



module.exports = ->
  @World = World
