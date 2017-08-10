require! {
  './app-linter' : AppLinter
  'chalk' : {cyan, blue}
  'js-yaml' : yaml
  'fs'
}

module.exports = ->
  if process.argv[2] is \help
    return help!
  app-config = yaml.safe-load fs.read-file-sync('application.yml', 'utf8')
  app-linter = new AppLinter {app-config}
  errorMessage = app-linter.start!
  if errorMessage
    console.log errorMessage
    process.exit 1


function help
  help-text = """
  Usage: #{cyan "exocom-lint"}

  Lints the Exosphere application in the current directory.
  This linter checks that all messages indicated to be sent by services of the application are received by one or more other services in the application and vice versa.
  This command must be called in the root directory of the application.
  """
  console.log help-text
