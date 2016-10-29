require! {
  'chalk' : {cyan, dim, green, red, magenta}
  'docopt' : {docopt}
  'nitroglycerin' : N
  '../package.json' : {name, version}
  '../package.json' : {version}
  'path'
  './exocom' : ExoCom
}


console.log dim "Exosphere Development Communications server #{version}\n"

doc = """
Provides Exosphere communication infrastructure services in development mode.

Usage:
  #{name} [EXOCOM_ZMQ_PORT=<port>] [EXOCOM_HTTP_PORT=<port>]
  #{name} -h | --help
  #{name} -v | --version
"""

on-zmq-bound = (port) ->
  console.log dim "Ctrl-C to stop"
  console.log "ExoCom #{version} ZMQ service online at port #{cyan port}"

on-http-bound = (port) ->
  console.log dim "Ctrl-C to stop"
  console.log "ExoCom #{version} HTTP service online at port #{magenta port}"

on-error = (err) ->
  console.log red "Error: #{err}"
  process.exit 1


run = ->
  exocom = new ExoCom service-messages: process.env.SERVICE_MESSAGES
    ..on 'zmq-bound', on-zmq-bound
    ..on 'http-bound', on-http-bound
    ..on 'error', on-error
    ..listen zmq-port: (+process.env.EXOCOM_ZMQ_PORT or 4100), http-port: (+process.env.EXOCOM_HTTP_PORT or 4101)
    ..on 'routing-setup', ->
      console.log 'receiving routing setup:'
      for command, routing of exocom.client-registry.routes
        process.stdout.write "  --[ #{command} ]-> "
        text = for receiver in routing.receivers
          "#{receiver.name} (#{receiver.host}:#{receiver.port})"
        process.stdout.write "#{text.join ' + '}\n"

    ..on 'message', ({messages, receivers}) ->
      for message in messages
        response-time = ''
        if message.response-to
          response-time = "  (#{(message.response-time * 1e-6).to-fixed 2} ms)"
        if message.name is message.original-name
          console.log "#{message.sender}  --[ #{message.name} ]->  #{receivers.join ' and '}#{response-time}"
        else
          console.log "#{message.sender}  --[ #{message.original-name} ]-[ #{message.name} ]->  #{receivers.join ' and '}#{response-time}"
        console.log message.payload


options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>
| otherwise                              =>  run!
