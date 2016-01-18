require! {
  \chalk : {cyan, dim, green}
  'docopt' : {docopt}
  '../package.json' : {name, version}
  'path'
  './exocomm' : ExoComm
}

console.log dim "Exosphere Development Communications server #{version}\n"

doc = """
Provides Exosphere communication infrastructure services in development mode.

Usage:
  #{name} run [--port=<port>]
  #{name} -h | --help
  #{name} -v | --version
"""

options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>
| options.run                            =>  new ExoComm!.listen options['--port'], (port) ->
                                               console.log dim "Ctrl-C to stop"
                                               console.log "online at port #{cyan port}"
| otherwise                              =>  console.err 'unhandled option'
