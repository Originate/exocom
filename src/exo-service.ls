require! {
  'events' : {EventEmitter}
  'exorelay' : ExoRelay
  'nitroglycerin' : N
  'portfinder'
  'rails-delegate' : {delegate, delegate-event}
  './service-loader'
}
debug = require('debug')('exocomm:cli')


class ExoService extends EventEmitter

  ({@root, @exocomm-port, @exorelay-port}) ->
    @exo-relay = new ExoRelay {@exocomm-port}
    delegate \close, from: @, to: @exo-relay
    delegate-event \online \offline \error, from: @exo-relay, to: @


  listen: ->
    service-loader @root, (service) ~>
      service.handlers.before-all ~>
        @exo-relay
          ..register-handlers service.handlers
          ..listen @exorelay-port


  get-port: (port, done) ->
    | port  =>  return done port
    portfinder.base-port = 3000
    portfinder.get-port host: 'localhost', (err, @port) ~>
      if err
        debug "Error getting port: #{err.message}"
        return @emit 'error', err
      done @port



module.exports = ExoService
