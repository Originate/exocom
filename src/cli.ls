require! {
  'chalk' : {cyan, dim, green, red}
  'docopt' : {docopt}
  'nitroglycerin' : N
  '../package.json' : {name, version}
  '../package.json' : {version}
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
  console.log "ExoComm #{version} online at port #{cyan port}"

on-error = (err) ->
  console.log red err
  process.exit 1


run = ->
  exocomm = new ExoComm!
    ..listen options['--port']
    ..on 'listening', on-listening
    ..on 'error', on-error
    ..on 'routing-setup', -> console.log 'receiving routing setup'
    ..on 'message', (message, receivers) -> console.log "broadcasting '#{message}' to the #{receivers.join ' and '}"


options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>
| otherwise                              =>  run!
