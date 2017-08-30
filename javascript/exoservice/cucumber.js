var common = [
  '--compiler ls:livescript',
  '-r features/support',
  '--fail-fast',
].join(' ');

module.exports = {
  "default": common
};
