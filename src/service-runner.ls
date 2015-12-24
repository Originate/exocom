require! {
  \chalk : {cyan, dim}
  \fs
  \http
  \nitroglycerin : N
  \path
  'robust-callbacks': roca
  './service-loader' : {load-service}
}



# Runs the service in the given directory
run-service = ({port}) ->
  load-service (service) ->
    service.handlers.before-all N ->
      listen {port}


handle-request = (req, res) ->
  res.write-head 200, 'Content-Type': 'text/plain'
  res.end 'hello world\n'


listen = ({port}) ->
  port ?= 3000
  http.create-server handle-request
      .listen port, '127.0.0.1', ->
        console.log "online at port #{cyan port}"
        console.log dim "Ctrl-C to stop"



module.exports = {run-service}
