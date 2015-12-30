require! {
  'chalk' : {cyan, dim, red}
  'express'
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


handle-homepage = (req, res) ->
  res.send 'hello world\n'



listen = ({port}) ->
  port ?= 3000
  app = express!
    ..get '/', handle-homepage
  server = app.listen port, ->
    console.log dim "Ctrl-C to stop"
    console.log "online at port #{cyan port}"



module.exports = {run-service}
