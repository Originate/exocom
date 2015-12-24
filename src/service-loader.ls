# Loads a service directory and makes it available as a convenient JS object

require! {
  \livescript
  \path
}

load-service = (done) ->
  handlers = require path.join(process.cwd!, 'src' 'server.ls')
  add-missing-handlers handlers
  done {handlers}


# Adds empty handlers to the given handlers object
# so that we can call them without null checks
add-missing-handlers = (handlers) ->
  # each ([it] ?= (cb) -> cb!) <[ before-all before after ]>
  handlers.before-all ?= -> it!
         ..before     ?= -> it!
         ..after      ?= -> it!


# Verifies that the given root directory contains an Exosphere service
verify-is-service = (done) ->
  fs.stat process.cwd!, (err, stats) ->
    | err?.code is 'ENOENT'   =>  return console.log "Error: '#{root}' is not a directory"
    | not stats.isDirectory!  =>  return console.log "Please provide the directory in which your service is"
    done!


module.exports = {load-service}
