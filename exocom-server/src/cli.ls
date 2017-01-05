require! {
  './cli-logger' : CliLogger
  'docopt' : {docopt}
  '../package.json' : {name, version}
  './exocom' : ExoCom
}

logger = new CliLogger

logger.log-header "Exocom #{version}\n"

doc = """
Provides Exosphere communication infrastructure services in development mode.
Expects the following environment variables:
- SERVICE_ROUTES: list of messages that the different service types are allowed to send and receive
- PORT: the port to listen on

Usage:
  #{name}
  #{name} -h | --help
  #{name} -v | --version
"""

switch options = docopt doc, help: no
| options['-h'] or options['--help']     =>  logger.log doc
| options['-v'] or options['--version']  =>
| otherwise                              =>  run!



function run
  exocom = new ExoCom service-routes: process.env.SERVICE_ROUTES
    ..on 'error', logger.log-error
    ..on 'http-online', logger.log-http-online
    ..on 'message', logger.log-message
    ..on 'routing-setup', logger.log-routing-setup
    ..on 'warn', logger.log-warn
    ..on 'websockets-online', logger.log-websockets-online
    ..listen (+process.env.PORT or 3100)

