require! {
  \chalk : {cyan, dim, green, red}
  'docopt' : {docopt}
  '../package.json' : {name, version}
  'path'
  './exo-service' : ExoService
}

console.log dim "Exosphere Node.js service runner #{version}\n"

service-data = require path.resolve('./package.json')
console.log "Running #{green service-data.name} #{green service-data.version}\n"


doc = """
Runs Exosphere services written in Node.js.

Usage:
  #{name} run [--port=<port>] [--exocomm-port=<exocomm-port>]
  #{name} -h | --help
  #{name} -v | --version
"""

run = (options) ->
  new ExoService exocomm-port: options['--exocomm-port']
    ..on 'online', (port) ->
      console.log dim "Ctrl-C to stop"
      console.log "online at port #{cyan port}"
    ..on 'error', (err) -> console.log red err
    ..on 'offline', -> console.log red 'SERVER CLOSED'
    ..listen port: options['--port']


options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>
| options.run                            =>  run options
| otherwise                              =>  console.err 'unhandled option'
