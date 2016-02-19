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
  exo-js run --name=<name> --exorelay-port=<exorelay-port> --exocomm-port=<exocomm-port>
  exo-js -h | --help
  exo-js -v | --version
"""

run = (options) ->
  new ExoService service-name: options['--name'], exocomm-port: options['--exocomm-port'], exorelay-port: options['--exorelay-port']
    ..on 'online', (port) ->
      console.log dim "Ctrl-C to stop"
      console.log "online at port #{cyan port}"
    ..on 'error', (err) -> console.log red err
    ..on 'offline', -> console.log red 'SERVER CLOSED'
    ..listen!


options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>
| options.run                            =>  run options
| otherwise                              =>  console.err 'unhandled option'
