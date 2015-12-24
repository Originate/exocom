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



module.exports = {load-service}
