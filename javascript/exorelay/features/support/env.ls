require! {
  'cucumber': {After, Before, set-default-timeout}
}

set-default-timeout 1000


After ->
  @exo-relay?.close!
  @exocom?.close!

Before ->
  @ran = no
