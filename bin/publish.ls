require! {
  'chalk' : {bold, red, cyan, yellow, green}
  'inquirer'
  'jsonfile'
  'semver'
  'shelljs/global'
}

{name, version} = jsonfile.readFileSync './exocom-server/package.json' # not sure about this, using exocom-server b/c there is no root package.json

if process.argv.length != 3
  display-help!

level = process.argv[2]
display-help! if level in ['-h, --help']

target-version = semver.inc version, level
unless target-version
  console.log "\n#{bold red 'Error:'} #{bold cyan level} #{red 'is not a valid version increment'}"
  display-help!

console.log "\nYou are about to bump #{green bold name} version #{cyan bold version} up to #{cyan bold target-version}\n"

question =
  type: 'list'
  name: 'continue'
  message: 'Are you sure?'
  choices: ['yes', 'no']
inquirer.prompt([question]).then (answer) ->
  if answer.continue == 'no'
    console.log '\nAborting ...\n'
    process.exit!

  console.log!

  # Ensure no open files
  open-files = exec "git status --porcelain", {silent: true}
  if open-files then console.log red 'Please commit all files before releasing' ; process.exit 1

  # Ensure on master
  current-branch = exec "git rev-parse --abbrev-ref HEAD", {silent: true}
  if current-branch.trim! isnt 'master' then console.log red 'You must be on the master branch to publish' ; process.exit 1

  check-npm-dependencies!
  build-subprojects!
  run-tests!
  bump-version-numbers!
  push-version-numbers!
  publish-to-npm!
  push-exocom-docker-image!
  push-git-tag!



published-directories =
  'exocom-mock-javascript'
  'exocom-server'
  'exorelay-javascript'
  'exoservice-javascript'


function check-npm-dependencies
  console.log green "Checking npm dependencies...\n"
  run-command-in-subdirs do
    command: './node_modules/.bin/update-check'
    command-message: 'Checking dependencies'
  console.log!


function build-subprojects
  console.log green "Building subprojects...\n"
  run-command-in-subdirs do
    command: './bin/build'
    command-message: 'Building'
  console.log!


function run-tests
  console.log green "Running tests in subprojects...\n"
  run-command-in-subdirs do
    command: './bin/spec'
    command-message: 'Running tests in'
  console.log!


function bump-version-numbers
  console.log green "Bumping subproject version numbers...\n"
  run-command-in-subdirs do
    command: "npm version #{level}"
    command-message: 'Bumping'
  console.log!


function push-version-numbers
  console.log green "Pushing version numbers...\n"
  run-command "git add -u && git commit -m #{target-version} && git push"
  console.log!


function publish-to-npm
  console.log green "Publishing to npm...\n"
  run-command-in-subdirs do
    command: 'npm publish'
    command-message: 'Publishing'
  console.log!


function push-exocom-docker-image
  console.log green "Pushing ExoCom image to DockerHub...\n"
  cd 'exocom-server'
  run-command "docker build --no-cache -t originate/exocom:#{target-version}"
  run-command "docker push originate/exocom:#{target-version}"
  cd '..'


function push-git-tag
  console.log green "Pushing git release tag...\n"
  run-command 'git tag -a v#{target-version} && git push --tags'
  console.log!


function run-command command
  if exec(command).code > 0 then process.exit 1


function run-command-in-subdirs {command, command-message}
  for directory in published-directories
    console.log "#{command-message} subproject #{cyan directory}"
    cd directory
    if exec(command).code > 0
      process.exit 1
    cd '..'
    console.log!


function display-help
  console.log "\nUsage:\n\n  #{bold 'publish-exocom <patch|minor|major>'}\n"
  process.exit 1
