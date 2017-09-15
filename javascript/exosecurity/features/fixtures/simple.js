const { bootstrap } = require('../..')

bootstrap((message, requester, callback) => {
  if (message.name === 'create user') {
    callback(message.auth === 'abc')
  } else {
    callback(false)
  }
})
