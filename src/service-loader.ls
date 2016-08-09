# Loads an Exoservice directory and makes it available
# as a convenient JS object

require! {
  'defaults'
  'glob'
  'livescript'
  'nitroglycerin' : N
  'path'
}


load-service = (root = '', done) ->
  _find-server-path root, (server-file-path) ->
    console.log server-file-path
    handlers = require server-file-path
    if not handlers.before-all? then handlers.before-all = -> it!
    done {handlers}


_find-server-path = (root, done) ->
  glob path.join(process.cwd!, root, 'server.*'), N (files) ->
    | files.length > 1   =>  throw _multi-files-error files
    | files.length is 1  =>  return done files[0]
    glob path.join(process.cwd!, root, '**', 'server.*'), N (files) ->
      | files.length > 1   =>  throw _multi-files-error files
      | files.length is 1  =>  return done files[0]
      throw new Error "Cannot find server file.
                       It must be named 'server.js' or comparably for your language."


_multi-files-error = (files) ->
  new Error "Multiple server files found: #{files.join ', '}"


module.exports = load-service
