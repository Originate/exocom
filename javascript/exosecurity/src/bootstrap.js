const { cyan, dim, red } = require('chalk')
const ExoSecurity = require('./exosecurity')

function bootstrap(messageAuthorizer) {
  const exosecurity = new ExoSecurity({
    exocomHost:
      process.env.EXOCOM_HOST != null ? process.env.EXOCOM_HOST : 'localhost',
    exocomPort: process.env.EXOCOM_PORT != null ? process.env.EXOCOM_PORT : 80,
    role: process.env.ROLE,
  })
  exosecurity.on('online', port => {
    console.log(dim('Ctrl-C to stop')) // eslint-disable-line no-console
    console.log(`online at port ${cyan(port)}`) // eslint-disable-line no-console
  })
  exosecurity.on('error', err => console.log(red(err))) // eslint-disable-line no-console
  exosecurity.on('offline', () => console.log(red('SERVER CLOSED'))) // eslint-disable-line no-console
  exosecurity.connect(messageAuthorizer)
}
module.exports = bootstrap
