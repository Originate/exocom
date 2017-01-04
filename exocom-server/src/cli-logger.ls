require! {
  'chalk' : {cyan, dim, green, red, magenta, yellow}
}


# Logs events in a human-friedly format to the CLI
class CliLogger

  log: (text) ~>
    console.log text


  write: (text) ~>
    process.stdout.write text


  log-header: (text) ->
    @log dim text


  log-websockets-online: (port) ~>
    @log "ExoCom WebSocket listener online at port #{cyan port}"


  log-http-online: (port) ~>
    @log "ExoCom HTTP service online at port #{magenta port}"


  log-error: (err) ~>
    @log red "Error: #{err}"
    process.exit 1


  log-warn: (warning) ~>
    @log yellow "Warning: #{warning}"


  log-message: ({messages, receivers}) ~>
    for message in messages
      response-time = ''
      if message.response-to
        response-time = "  (#{(message.response-time * 1e-6).to-fixed 2} ms)"
      if message.name is message.original-name
        @log "#{message.sender}  --[ #{message.name} ]->  #{receivers.join ' and '}#{response-time}"
      else
        @log "#{message.sender}  --[ #{message.original-name} ]-[ #{message.name} ]->  #{receivers.join ' and '}#{response-time}"
      @log message.payload


  log-routing-setup: ~>
    @log 'receiving routing setup:'
    for command, routing of exocom.client-registry.routes
      @write "  --[ #{command} ]-> "
      text = for receiver in routing.receivers
        "#{receiver.name}"
      @write "#{text.join ' + '}\n"



module.exports = CliLogger
