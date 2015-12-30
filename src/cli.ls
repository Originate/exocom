require! {
  \chalk : {dim}
  'docopt' : {docopt}
  '../package.json' : {name, version}
  './service-runner' : {run-service}
}

doc = """
Runs Exosphere services written in Node.js.

Usage:
  #{name} run [--port=<port>]
  #{name} -h | --help
  #{name} -v | --version
"""

console.log dim "Exosphere Node.js service runner #{version}\n"
options = docopt doc, help: no
switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>  return
| options.run                            =>  run-service port: options['--port']
| _                                      =>  return console.err 'unhandled option'
