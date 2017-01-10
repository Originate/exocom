require! {
  'events' : {EventEmitter}
  'exorelay' : ExoRelay
  'nitroglycerin' : N
  'rails-delegate' : {delegate, delegate-event}
  './service-loader'
}
debug = require('debug')('exocom:cli')


class ExoService extends EventEmitter

  ({@root, exocom-host, exocom-port, role, internal-namespace}) ->
    @exo-relay = new ExoRelay {
      exocom-host
      exocom-port
      role
      internal-namespace
    }
    delegate \close, from: @, to: @exo-relay
    delegate-event \online \offline \error, from: @exo-relay, to: @


  connect: ->
    service = service-loader @root
    service.handlers.before-all ~>
      @exo-relay
        ..connect!
        ..register-handlers service.handlers



module.exports = ExoService
