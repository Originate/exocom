var common = [
  '--compiler ls:livescript',
  '-r features',
  '--fail-fast',
].join(' ');

module.exports = {
  "default": common
};
