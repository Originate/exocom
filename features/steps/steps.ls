require! {
  'chai' : {expect}
  'jsdiff-console'
  'http'
  'livescript'
  'observable-process' : ObservableProcess
  'request'
  'wait' : {wait-until}
}


module.exports = ->

  @Given /^an ExoComm instance at port (\d+)$/, (@exocomm-port, done) ->
    @process = new ObservableProcess "bin/exocomm run --port #{@exocomm-port}", verbose: yes
      ..wait "online at port #{@exocomm-port}", done


  @Given /^another service already uses port (\d+)$/, (port, done) ->
    handler = (_, res) -> res.end 'existing server'
    @existing-server = http.create-server(handler).listen port, ->
      request "http://localhost:3100", (err, _, body) ->
        expect(err).to.be.null
        expect(body).to.equal 'existing server'
        done!


  @Given /^it knows about this service:$/, (service-data, done) ->
    eval livescript.compile "payload = {\n#{service-data}\n}", bare: yes, header: no
    request-data =
      url: "http://localhost:4100/register-service",
      method: 'POST'
      body:
        payload: payload
      json: yes
    request request-data, done



  @When /^I run "([^"]*)"$/, (code) ->
    @process = new ObservableProcess code, verbose: no


  @When /^receiving a service registration via this request:$/, (request-data, done) ->
    eval livescript.compile "data = {\n#{request-data}\n}", bare: yes, header: no
    data.json = yes
    request data, (err, {status-code}) ->
      expect(err).to.be.null
      expect(status-code).to.equal 200
      done!


  @Then /^it aborts$/, (done) ->
    wait-until (~> @process.crashed), done


  @Then /^it knows about these services:$/, (service-data, done) ->
    eval livescript.compile "expected-services = [\n#{service-data}\n]", bare: yes, header: no
    request "http://localhost:#{@exocomm-port}/status.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      parsed-body = JSON.parse body
      jsdiff-console parsed-body.clients, expected-services, done


  @Then /^this service runs at port (\d+)$/, (port, done) ->
    @process.wait "online at port #{port}", done
