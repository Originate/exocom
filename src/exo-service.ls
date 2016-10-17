require! {
  'events' : {EventEmitter}
  'exorelay' : ExoRelay
  'nitroglycerin' : N
  'rails-delegate' : {delegate, delegate-event}
  './service-loader'
}
debug = require('debug')('exocom:cli')


class ExoService extends EventEmitter

  ({@root, @exorelay-port, exocom-host, exocom-port, service-name, internal-namespace}) ->
    @exo-relay = new ExoRelay {
      @exorelay-port
      exocom-host
      exocom-port
      service-name
      internal-namespace
    }
    delegate \close \closePort, from: @, to: @exo-relay
    delegate-event \online \offline \error, from: @exo-relay, to: @


  listen: ->
    service = service-loader @root
    service.handlers.before-all ~>
      @exo-relay
        ..register-handlers service.handlers
        ..listen @exorelay-port



module.exports = ExoService
