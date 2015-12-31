require! {
  'body-parser'
  'chalk' : {green, red}
  'express'
  'fs'
  'path'
  'robust-callbacks': roca
  './service-loader'
}
roca.set-timeout 100


# the exoservice we are executing
service = null


homepage-controller = (req, res) ->
  res.send 'hello world\n'


command-controller = (req, res) ->
  command = req.params.command
  run-command command, service.handlers[command], req, res, ({status, message}) ->
    match status
    | 200  =>  console.log green "Processed command '#{command}'"
    | _    =>  console.log red "Problem with command '#{command}': #{message}"
    res.status(status).send message


# Runs the given command, returns the result asynchronously in all cases,
# even if an exception is thrown.
run-command = (command-name, handler, req, res, done) ->
  | typeof handler is 'undefined'  =>  return done status: 404, message: 'Unknown command'
  try
    handler req, roca (err) ->
      | err  =>  done status: 500, message: err.message
      | _    =>  done status: 200, message: ''
  catch
    done status: 500, message: e.message


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
