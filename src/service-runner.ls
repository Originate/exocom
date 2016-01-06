require! {
  'body-parser'
  'chalk' : {green, red}
  'express'
  'fs'
  'path'
  './service-loader'
}


# the exoservice we are executing
service = null


homepage-controller = (req, res) ->
  res.send 'hello world\n'


command-controller = (req, res) ->
  command = req.params.command
  unless (handler = service.handlers[command])
    console.log red "unknown command: '#{command}'"
    return res.status(404).send ''
  res.status(200).send ''
  handler req


add-routes = (app) ->
  app.get  '/'            , homepage-controller
    ..post '/run/:command', command-controller


run-before-all = (done) ->
  service.handlers.before-all (err) ->
    | err  =>  throw new Error "Error in before-all handler: #{err}"
    | _    =>  done!


start-express-app = (port, done) ->
  app = express!
    ..use body-parser.json!
  add-routes app
  app.listen port, done


# Runs the service in the given directory
run-service = ({port}, done) ->
  service-loader (srvc) ->
    service := srvc
    run-before-all ->
      start-express-app port, ->
        done port


module.exports = run-service
