require! {
  'events' : {EventEmitter}
  'exorelay' : ExoRelay
  'nitroglycerin' : N
  'rails-delegate' : {delegate, delegate-event}
  './service-loader'
}
debug = require('debug')('exocom:cli')


class ExoService extends EventEmitter

  ({@root, @exocom-port, @exorelay-port, @service-name}) ->
    @exo-relay = new ExoRelay {@exocom-port, @service-name}
    delegate \close, from: @, to: @exo-relay
    delegate-event \online \offline \error, from: @exo-relay, to: @


  listen: ->
    service-loader @root, (service) ~>
      service.handlers.before-all ~>
        @exo-relay
          ..register-handlers service.handlers
          ..listen @exorelay-port



module.exports = ExoService
