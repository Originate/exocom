require! {
  'chalk' : {cyan, dim, green, red}
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
  #{name} [--port=<port>]
  #{name} -h | --help
  #{name} -v | --version
"""

on-listening = (port) ->
  console.log dim "Ctrl-C to stop"
  console.log "ExoCom #{version} online at port #{cyan port}"

on-error = (err) ->
  console.log red err
  process.exit 1


run = ->
  exocom = new ExoCom!
    ..listen options['--port']
    ..on 'listening', on-listening
    ..on 'error', on-error
    ..on 'routing-setup', ->
      console.log 'receiving routing setup:'
      for command, routing of exocom.client-registry.routes
        process.stdout.write "  --[ #{command} ]-> "
        text = for receiver in routing.receivers
          "#{receiver.name} (#{receiver.host}:#{receiver.port})"
        process.stdout.write "#{text.join ' + '}\n"

    ..on 'message', ({message, receivers}) ->
      console.log "#{message.sender}  --[ #{message.name} ]->  #{receivers.join ' and '}"
      console.log message.payload


options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>
| otherwise                              =>  run!
