debug = require('debug')('exo-server')
roca = require 'robust-callbacks'
require! 'http'

hostname = '127.0.0.1'


handle-request = (req, res) ->
  res.write-head 200, 'Content-Type': 'text/plain'
  res.end 'hello world\n'


listen = ({port, setup}) ->
  setup (err) ->
    | err  =>  throw new Error err

    http.create-server handle-request
        .listen port, hostname, ->
          debug "online at http://#{hostname}:#{port}/"



module.exports = {listen}
