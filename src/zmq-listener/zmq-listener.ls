require! {
  'events' : {EventEmitter}
  'zmq'
}
debug = require('debug')('exorelay:zmq-listener')


# The ZMQ endpoint into which the Exosphere environment can
# PUSH new messages.
#
# Emits these events:
# - online
# - offline
# - error
class ZmqListener extends EventEmitter

  ->
    @zmq-socket = null
    @port = null


  # Closes the port that ExoRelay is listening on
  close: ->
    return unless @zmq-socket
    debug "no longer listening at port #{@port}"
    @zmq-socket.close!
    @zmq-socket = null
    @port = null
    @emit 'offline'


  listen: (+@port) ->
    | isNaN @port  =>  return @emit 'error', Error 'Non-numerical port provided to ExoRelay#listen'

    @zmq-socket = zmq.socket 'pull'
      ..on 'message', @on-zmq-socket-message
      ..on 'bind_error' (file_descriptor, endpoint) ~>
        @emit 'error', "port #{endpoint.substring 2} for ExoRelay is already in use"
      ..monitor 500
    try
      @zmq-socket.bind-sync "tcp://*:#{@port}"
      @emit 'online', @port
    catch
      console.log e.message


  on-zmq-socket-message: (data) ~>
    request-data = @_parse-request JSON.parse data.to-string!
    @_log request-data
    switch (result = @listeners('message')[0] request-data)
      | 'success'             =>
      | 'missing message id'  =>  @emit 'error', Error 'missing message id'
      | 'unknown message'     =>  @emit 'error', Error "unknown message: '#{request-data.message-name}'"
      | _                     =>  @emit 'error', Error "unknown result code: '#{@result}'"


  _log: ({message-name, id, response-to}) ->
    | response-to  =>  debug "received message '#{message-name}' with id '#{id}' in response to '#{response-to}'"
    | _            =>  debug "received message '#{message-name}' with id '#{id}'"


  # Returns the relevant data from a request
  _parse-request: (req) ->
    message-name = req.name
    payload = req.payload
    response-to = req.response-to
    id = req.id
    {message-name, response-to, payload, id}



module.exports = ZmqListener
