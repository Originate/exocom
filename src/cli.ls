require! {
  \chalk : {cyan, dim}
  'docopt' : {docopt}
  '../package.json' : {name, version}
  './service-runner'
}

console.log dim "Exosphere Node.js service runner #{version}\n"

doc = """
Runs Exosphere services written in Node.js.

Usage:
  #{name} run [--port=<port>]
  #{name} -h | --help
  #{name} -v | --version
"""
options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>  return
| options.run                            =>  service-runner port: options['--port'] || 3000, (port) ->
                                               console.log dim "Ctrl-C to stop"
                                               console.log "online at port #{cyan port}"
| otherwise                              =>  return console.err 'unhandled option'
