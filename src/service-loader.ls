# Loads an Exoservice directory and makes it available
# as a convenient JS object

require! {
  'defaults'
  'livescript'
  'path'
}

load-service = (done) ->
  handlers = require path.join(process.cwd!, 'src', 'server.ls')
  if not handlers.before-all? then handlers.before-all = (done) -> done!
  done {handlers}



module.exports = load-service
