require! {
  'cucumber': {After, Before, set-default-timeout, set-world-constructor}
  'exosphere-shared' : {kill-child-processes}
  './world': World
}


set-default-timeout 1000
set-world-constructor World


After (scenario, done) ->
  @server1?.close!
  @server2?.close!
  @process?.kill!
  closeIfDefined @exoservice, ~>
    closeIfDefined @exocom, ~>
      kill-child-processes done


Before ->
  @ran = no


closeIfDefined = (obj, done) ->
  if obj
    obj.close done
  else
    done!
