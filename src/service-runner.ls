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


add-routes = (app) ->
  app.get  '/'            , handle-homepage
    ..post '/run/:command', handle-command


run-before-all = (done) ->
  service.handlers.before-all (err) ->
    | err  =>  console.log red "Error in before-all handler: #{err}"
    | _    =>  done!


start-express-app = (port, done) ->
  app = express!
  add-routes app
  app.listen port, done


# Runs the service in the given directory
run-service = ({port}) ->
  load-service (srvc) ->
    service := srvc
    run-before-all ->
      start-express-app port, ->
        console.log dim "Ctrl-C to stop"
        console.log "online at port #{cyan port}"



module.exports = run-service
