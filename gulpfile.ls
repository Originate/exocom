require! {
  \gulp
  \gulp-lint-ls
}


gulp.task 'lint', ->
  gulp.src './src/**/*.ls'
      .pipe gulp-lint-ls allow-throw: yes, allow-this: yes, allow-new: yes
