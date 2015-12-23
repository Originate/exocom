{name, version} = require '../package.json'
{docopt} = require 'docopt'

doc = """
Runs Exosphere services written in Node.js.

Usage:
  #{name} run <file>
  #{name} -h | --help
  #{name} -v | --version
"""

console.log "Exosphere Node.js service runner #{version}\n"

options = docopt doc, help: no

switch
| options['-h'] or options['--help']     =>  console.log doc
| options['-v'] or options['--version']  =>  return
| options.run                            =>  run_server options['<file>']
| _                                      =>  return console.err 'unhandled option'
