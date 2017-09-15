const bootstrap = require('../..').bootstrap

bootstrap((message, requester, callback) => {
  if (message.name === 'create user') {
    requester(
      'retrieve user session',
      message.auth,
      (receivedName, receivedPayload) => {
        if (receivedName === 'user session retrieved') {
          callback(receivedPayload.isAdmin === 'true')
        } else {
          callback(false)
        }
      }
    )
  } else {
    callback(false)
  }
})
