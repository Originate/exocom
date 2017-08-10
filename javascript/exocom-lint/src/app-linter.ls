require! {
  'async'
  'events' : {EventEmitter}
  'fs'
  'path'
  'prelude-ls' : {difference, find, filter, reject, each}
  'js-yaml' : yaml
}

class AppLinter extends EventEmitter

  ({@app-config}) ->


  start: ->
    {sent-messages, received-messages} = @aggregate-messages!
    @lint-messages sent-messages, received-messages


  lint-messages: (sent, received) ->
    not-received = difference Object.keys(sent), Object.keys(received)
    not-sent = difference Object.keys(received), Object.keys(sent)
    if not-received.length is 0 and not-sent.length is 0
      return ""
    errorMessage = ""
    if not-received.length
      errorMessage += "The following messages are sent but not received:\n"
      for msg in not-received
        errorMessage += "  #{sent[msg]}: #{msg}\n"
    if not-sent.length
      errorMessage += "The following messages are received but not sent:\n"
      for msg in not-sent
        errorMessage += "  #{received[msg]}: #{msg}\n"
    errorMessage


  aggregate-messages: ->
    sent-messages = {}
    received-messages = {}
    for protection-level of @app-config.services
      for service-role, service-data of @app-config.services[protection-level]
        service-config = @get-config service-data
        for message in service-config.messages.sends or []
          public-message = @get-public-message message, service-data
          (sent-messages[public-message] or= []).push service-role
        for message in service-config.messages.receives or []
          public-message = @get-public-message message, service-data
          (received-messages[public-message] or= []).push service-role
    {sent-messages, received-messages}


  get-public-message: (message, service-data) ->
    for translation in service-data.message-translation or []
      if message is translation.internal then return translation.public
    message


  get-config: (service-data) ->
    yaml.safe-load fs.read-file-sync(path.join(process.cwd!, service-data.location, 'service.yml'), 'utf8')


module.exports = AppLinter
