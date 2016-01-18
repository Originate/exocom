require! {
  './http-listener' : HttpListener
}


class ExoComm

  ->
    @http-listener = new HttpListener


  listen: (port, done) ->
    port or= 3100
    @http-listener.listen port, ->
      done port



module.exports = ExoComm
