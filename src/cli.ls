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
  #{name} [--zmq-port=<port>] [--http-port=<port>]
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
  exocom = new ExoCom!
    ..on 'zmq-bound', on-zmq-bound
    ..on 'http-bound', on-http-bound
    ..on 'error', on-error
    ..listen zmq-port: (options['--zmq-port'] or 4100), http-port: (options['--http-port'] or 4101)
    ..on 'routing-setup', ->
      console.log 'receiving routing setup:'
      for command, routing of exocom.client-registry.routes
        process.stdout.write "  --[ #{command} ]-> "
        text = for receiver in routing.receivers
          "#{receiver.name} (#{receiver.host}:#{receiver.port})"
        process.stdout.write "#{text.join ' + '}\n"

    ..on 'message', ({messages, receivers}) ->
      for message in messages
        if message.name is message.original-name
          console.log "#{message.sender}  --[ #{message.name} ]->  #{receivers.join ' and '}"
        else
          console.log "#{message.sender}  --[ #{message.original-name} ]-[ #{message.name} ]->  #{receivers.join ' and '}"
        console.log message.payload


options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>
| otherwise                              =>  run!
