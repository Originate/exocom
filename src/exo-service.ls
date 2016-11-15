require! {
  'events' : {EventEmitter}
  'exorelay' : ExoRelay
  'nitroglycerin' : N
  'rails-delegate' : {delegate, delegate-event}
  './service-loader'
}
debug = require('debug')('exocom:cli')


class ExoService extends EventEmitter

  ({@root, exocom-host, exocom-port, service-name, internal-namespace}) ->
    @exo-relay = new ExoRelay {
      exocom-host
      exocom-port
      service-name
      internal-namespace
    }
    delegate \close, from: @, to: @exo-relay
    delegate-event \online \offline \error, from: @exo-relay, to: @


  listen: ->
    service = service-loader @root
    service.handlers.before-all ~>
      @exo-relay
        ..connect!
        ..register-handlers service.handlers



module.exports = ExoService
