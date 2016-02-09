# Loads an Exoservice directory and makes it available
# as a convenient JS object

require! {
  'defaults'
  'fs'
  'livescript'
  'path'
}


find-server-path = (root, done) ->
  fs.stat path.join(process.cwd!, root, 'server.ls'), (err, stats) ->
    | !err  =>  return done path.join(process.cwd!, root, 'server.ls')
    fs.stat path.join(process.cwd!, root, 'src', 'server.ls'), (err, stats) ->
      | !err  =>  return done path.join(process.cwd!, root, 'src', 'server.ls')
      threw new Error "Cannot find server.ls file"


load-service = (root = '', done) ->
  find-server-path root, (server-file-path) ->
    handlers = require server-file-path
    if not handlers.before-all? then handlers.before-all = -> it!
    done {handlers}



module.exports = load-service
