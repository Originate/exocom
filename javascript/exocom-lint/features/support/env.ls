require! {
  './kill-child-processes'
  './world': World
  'cucumber': {set-default-timeout, set-world-constructor, After}
}


set-default-timeout 2000


set-world-constructor World


After (scenario, done) ->
  kill-child-processes done
