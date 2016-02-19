require! {
  'events' : {EventEmitter}
  'exorelay' : ExoRelay
  'nitroglycerin' : N
  'rails-delegate' : {delegate, delegate-event}
  './service-loader'
}
debug = require('debug')('exocomm:cli')


class ExoService extends EventEmitter

  ({@root, @exocomm-port, @exorelay-port, @service-name}) ->
    @exo-relay = new ExoRelay {@exocomm-port, @service-name}
    delegate \close, from: @, to: @exo-relay
    delegate-event \online \offline \error, from: @exo-relay, to: @


  listen: ->
    service-loader @root, (service) ~>
      service.handlers.before-all ~>
        @exo-relay
          ..register-handlers service.handlers
          ..listen @exorelay-port



module.exports = ExoService
