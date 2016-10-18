require! {
  'events' : {EventEmitter}
  'zmq'
}
debug = require('debug')('exocom:zmq-listener')


# The ZeroMQ socket endpoint that listens for messages that services want to send
#
# Emits these events:
# - error: when it cannot bind to the given port
# - bound: when it listens at the given port
class ZMQListener extends EventEmitter

  (@exocom) ->
    @responder = zmq.socket 'pull'
      ..on 'message', @on-socket-receive
    @port = null


  close: ->
    | !@responder  =>  return
    debug "ZMQ no longer bound at port #{@port}"
    @responder.close!


  listen: (+@port) ->
    | isNaN @port  =>  @emit 'error', 'Non-numerical port provided to ExoCom#listen'
    @responder.on 'bind_error', (file_descriptor, endpoint) ~>
      @emit 'error', "port #{endpoint.substring 2} is already in use"
    @responder.monitor!
    # An exception will be thrown if the port is being used, so
    # the exception needs to be caught but we dont crash so that we get the correct error
    try
      @responder.bind-sync "tcp://*:#{@port}"
      @emit 'zmq-bound', @port
    catch
      console.log e.message


  on-socket-receive: (data) ~>
    request-data = @_parse-request JSON.parse data.to-string!
    @_log request-data
    if request-data.name is "exocom.register-service"
      @exocom.add-routing-config request-data.payload
    else
      switch (result = @exocom.send-message request-data)
        | 'success'             =>
        | 'missing request id'  =>  @emit 'error', 'missing request id'
        | 'unknown message'     =>  @emit 'error', "unknown message: '#{request-data.message}'"
        | _                     =>  @emit 'error', "unknown result code: '#{@result}'"


  _log: ({name, id, response-to}) ->
    | response-to  =>  debug "received message '#{name}' with id '#{id}' in response to '#{response-to}'"
    | _            =>  debug "received message '#{name}' with id '#{id}'"


  # Returns the relevant data from a request
  _parse-request: (req) ->
    sender = req.sender
    name = req.name
    payload = req.payload
    response-to = req.response-to
    timestamp = req.timestamp
    id = req.id
    {id, name, payload, response-to, sender, timestamp}



module.exports = ZMQListener
