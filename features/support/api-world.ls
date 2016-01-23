require! {
  '../../lib/exocomm' : ExoComm
  'chai' : {expect}
  'jsdiff-console'
  'wait' : {wait}
}


ApiWorld = !->

  @create-exocomm-instance = ({port}, done) ->
    @exocomm = new ExoComm
      ..listen port
      ..on 'listening', -> done!


  @register-service = (service-data, done) ->
    result = @exocomm.on-http-listener-register-service service-data[0]
    expect(result).to.equal 'success'
    done!


  @run-exocomm = (expect-error, done) ->
    @exocomm = new ExoComm
      ..listen null
    if expect-error
      @exocomm.on 'error', (@err) ~> done!
    else
      @exocomm.on 'listening', -> done!


  @run-exocomm-at-port = (port, expect-error, done) ->
    @exocomm = new ExoComm
      ..listen port
    if expect-error
      @exocomm.on 'error', (@err) ~> done!
    else
      @exocomm.on 'listening', -> done!


  @verify-abort-with-message = (message, done) ->
    process.next-tick ~>
      expect(@err).to.equal message
      done!


  @verify-knows-about-services = (service-data, done) ->
    @exocomm.on-http-listener-get-config ({clients}) ->
      jsdiff-console clients, service-data, done


  @verify-runs-at-port = (port, done) ->
    expect(@exocomm.port).to.equal port
    done!



module.exports = ->
  @World = ApiWorld if process.env.EXOCOMM_TEST_DEPTH is 'API'
