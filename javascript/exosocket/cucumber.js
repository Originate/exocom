const common = [
  '--compiler js:babel-register',
  `--format ${process.env.CI ? 'progress' : 'progress-bar'}`,
  '--format rerun:@rerun.txt',
  '--require features/support',
].join(' ')

module.exports = {
  default: common,
}
