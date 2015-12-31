require! {
  'chalk' : {cyan, dim, red}
  'express'
  'fs'
  'path'
  'robust-callbacks': roca
  './service-loader' : {load-service}
}


# the exoservice we are executing
service = null


handle-homepage = (req, res) ->
  res.send 'hello world\n'


handle-command = (req, res) ->
  match typeof (handler = service.handlers[req.params.command])
  | 'undefined'  =>  res.status(404).end!
  | _            =>  handler req, -> res.end!


# Runs the service in the given directory
run-service = ({port}) ->
  load-service (srvc) ->
    service := srvc
    service.handlers.before-all (err) ->
      | err  =>  return console.log red "Error in before-all handler: #{err}"
      app = express!
        ..get '/', handle-homepage
        ..post '/run/:command', handle-command
        ..listen port, ->
          console.log dim "Ctrl-C to stop"
          console.log "online at port #{cyan port}"



module.exports = run-service
