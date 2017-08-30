require! {
  'cucumber': {defineSupportCode}
  '../../../exocom-mock' : ExoComMock
  'http'
  'nitroglycerin': N
  'port-reservation'
  'wait' : {wait}
}


defineSupportCode ({Given}) ->

  Given /^an ExoCom instance$/, (done) ->
    port-reservation.get-port N (@exocom-port) ~>
      @exocom = new ExoComMock
        ..listen @exocom-port
      done!

  Given /^an ExoCom instance running at port (\d+)$/, (@exocom-port) ->
    @exocom = new ExoComMock
      ..listen @exocom-port


  Given /^an instance of the "([^"]*)" fixture$/, (@role, done) ->
    @create-exoservice-instance {@role, @exocom-port}, ~>
      @remove-register-service-message @exocom, done
