require! {
  'chai' : {expect}
  'jsdiff-console'
  'observable-process' : ObservableProcess
  'request'
  'wait' : {wait-until}
}


# Provides steps for end-to-end testing of the service as a stand-alone binary
E2EWorld = !->

  @create-exocomm-instance = ({port}, done) ->
    @process = new ObservableProcess "bin/exocomm run --port #{@exocomm-port}", verbose: yes
      ..wait "online at port #{port}", done


  @register-service = (service-data, done) ->
    request-data =
      url: "http://localhost:#{@exocomm-port}/register-service",
      method: 'POST'
      body:
        payload: service-data[0]
      json: yes
    request request-data, done


  @run-exocomm = (_expect-error, done) ->
    @process = new ObservableProcess 'bin/exocomm run', verbose: yes
    done!


  @run-exocomm-at-port = (port, _expect-error, done) ->
    @process = new ObservableProcess "bin/exocomm run --port #{port}", verbose: yes
    done!


  @verify-abort-with-message = (message, done) ->
    @process.wait message, ~>
      wait-until (~> @process.crashed), done


  @verify-knows-about-services = (service-data, done) ->
    request "http://localhost:#{@exocomm-port}/status.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      actual = JSON.parse body
      jsdiff-console actual.clients, service-data, done


  @verify-runs-at-port = (port, done) ->
    @process.wait "online at port #{port}", done



module.exports = ->
  @World = E2EWorld if process.env.EXOCOMM_TEST_DEPTH is 'E2E'
