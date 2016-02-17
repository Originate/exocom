require! {
  'chalk' : {cyan, dim, green, red}
  'docopt' : {docopt}
  'nitroglycerin' : N
  '../package.json' : {name, version}
  'path'
  './exocomm' : ExoComm
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
  console.log "online at port #{cyan port}"

on-error = (err) ->
  console.log red err
  process.exit 1


run = ->
  exocomm = new ExoComm!
    ..listen options['--port']
    ..on 'listening', on-listening
    ..on 'error', on-error
    ..on 'routing-setup', -> console.log 'receiving routing setup'
    ..on 'command', (command, receivers) -> console.log "broadcasting '#{command}' to the #{receivers.join ' and '}"


options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>
| otherwise                              =>  run!
