require! {
  'exorelay' : ExoRelay
  './service-loader'
}


# Runs the service in the given directory
run-service = ({port}, done) ->
  service-loader (service) ->
    exo-relay = new ExoRelay!
      ..register-handlers service.handlers
      ..listen port, ->
        done port


module.exports = run-service
