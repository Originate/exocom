require! {
  '../../lib/exocomm' : ExoComm
  'chai' : {expect}
  'jsdiff-console'
  'wait' : {wait}
}


ApiWorld = !->

  @create-exocomm-instance = ({port}, done) ->
    @exocomm = new ExoComm
      ..listen port, done


  @register-service = (service-data, done) ->
    result = @exocomm.register-service service-data[0]
    expect(result).to.equal 'success'
    done!


  @run-exocomm = (done) ->
    @exocomm = new ExoComm
      ..listen null, (@listen-error) ~>
        done!


  @run-exocomm-at-port = (port, done) ->
    @exocomm = new ExoComm
      ..listen port, (@listen-error) ~>
        done!


  @verify-abort-with-message = (message, done) ->
    process.next-tick ~>
      expect(@listen-error).to.equal message
      done!


  @verify-knows-about-services = (service-data, done) ->
    @exocomm.get-config ({clients}) ->
      jsdiff-console clients, service-data, done


  @verify-runs-at-port = (port, done) ->
    expect(@exocomm.port).to.equal port
    done!



module.exports = ->
  @World = ApiWorld if process.env.EXOCOMM_TEST_DEPTH is 'API'
