require! {
  \gulp
  \gulp-livescript
  \gulp-concat
  \gulp-exit
  \gulp-header
  \gulp-mocha
  \gulp-util
  \gulp-uglify
  \gulp-lint-ls
}
{instrument, hook-require, write-reports} = (require \gulp-livescript-istanbul)!


gulp.task 'default', ->
  {name, version, homepage} = require './package.json'
  banner = "/*! #{name} #{version}, \
            copyright #{new Date!getFullYear!} Originate, \
            see #{homepage} */\n"
  gulp.src './src/*.ls'
      .pipe gulp-livescript!
      .pipe gulp-uglify!
      .pipe gulp-concat 'asca.js'
      .pipe gulp-header banner
      .pipe gulp.dest 'lib'
      .on 'error', gulp-util.log


gulp.task 'lint', ->
  gulp.src './src/**/*.ls'
      .pipe gulp-lint-ls allow-throw: yes, allow-this: yes, allow-new: yes
