module.exports =

  hello: ({name}, response) ->
    response.send message: "Hello #{name}!"
