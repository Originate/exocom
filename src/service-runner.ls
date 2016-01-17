require! {
  'exorelay' : ExoRelay
  'nitroglycerin' : N
  'portfinder'
  './service-loader'
}


get-port = (port, done) ->
  | port  =>  return done null, port
  portfinder.base-port = 3000
  portfinder.get-port host: 'localhost', done


# Runs the service in the given directory
run-service = ({port, exocomm-port}, done) ->
  get-port port, N (port) ->
    service-loader (service) ->
      exo-relay = new ExoRelay!
        ..register-handlers service.handlers
        ..listen port, ->
          done port


module.exports = run-service
