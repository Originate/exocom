require! {
  'chai' : {expect}
  'cucumber': {Then}
}


Then /^it prints:$/, (expected-text) ->
  expect(@process.full-output!.trim!).to.eql expected-text.trim!


Then /^the exit code is (\d)$/, (number) ->
  expect(@process.exit-code).to.eql parseInt(number)
