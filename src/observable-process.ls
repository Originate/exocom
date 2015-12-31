require! {
  'child_process' : {spawn}
  'path'
  'prelude-ls' : {head, tail}
  'request'
}

# Cannot use nexpect, because it doesn't allow to add listeners to existing processes.
# Cannot use StreamSnitch because it is buggy and blocks the event queue.


# Spawns the given command into a separate, parallel process
# and allows to observe it.
class ObservableProcess

  (command, @options) ->
    command-parts = command.split ' '
    @process = spawn(path.join(process.cwd!, head command-parts),
                     tail(command-parts),
                     options)
      ..on 'close', @on-close
      ..stdout.on 'data', @on-output
      ..stdout.on 'error', @on-output

    # the strings to search for
    @searches = []

    # the output captured so far
    @output = ''

    # whether this process has been officially killed
    # (to avoid unnecessary panic if it ends)
    @killed = no


  kill: ->
    @killed = yes
    @process.kill!
    @process = undefined


  on-close: (err) ~>
    | @killed  =>  return
    console.log 'PROCESS CRASHED'
    console.log @output
    console.log "\nEXIT CODE: #{err}"


  # Called when new console output arrives
  on-output: (data) ~>
    text = data.toString!
    console.log text if @options.verbose
    @output += text
    @check-searches!


  # Looks for new matches in the received text.
  # Called each time the text or search terms change.
  check-searches: ->
    for i from @searches.length-1 to 0 by -1
      if @output.includes @searches[i].text
        @searches[i].handler!
        @searches.splice i, 1


  # Calls the given handler when the given text shows up in the output
  wait: (text, handler) ->
    @searches.push {text, handler}
    @check-searches!



module.exports = ObservableProcess
