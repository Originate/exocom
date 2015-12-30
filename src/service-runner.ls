require! {
  \http
  'chalk' : {cyan, dim, red}
  'fs'
  'path'
  'robust-callbacks': roca
  './service-loader' : {load-service}
}


# Runs the service in the given directory
run-service = ({port}) ->
  load-service (service) ->
    service.handlers.before-all (err) ->
      | err  =>  return console.log red "Error in before-all handler: #{err}"
      listen {port}


handle-request = (req, res) ->
  res.write-head 200, 'Content-Type': 'text/plain'
  res.end 'hello world\n'


listen = ({port}) ->
  port ?= 3000
  http.create-server handle-request
      .listen port, '127.0.0.1', ->
        console.log dim "Ctrl-C to stop"
        console.log "online at port #{cyan port}"



module.exports = {run-service}
