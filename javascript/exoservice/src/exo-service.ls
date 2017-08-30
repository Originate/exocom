require! {
  'events' : {EventEmitter}
  '../../exorelay' : ExoRelay
  'rails-delegate' : {delegate, delegate-event}
}


class ExoService extends EventEmitter

  ({exocom-host, exocom-port, @handlers, role}) ->
    @exo-relay = new ExoRelay {
      exocom-host
      exocom-port
      role
    }
    delegate \close, from: @, to: @exo-relay
    delegate-event \online \offline \error, from: @exo-relay, to: @


  connect: ->
    @handlers.before-all ~>
      @exo-relay
        ..connect!
        ..register-handlers @handlers


module.exports = ExoService
