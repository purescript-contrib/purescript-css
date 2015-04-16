var gulp = require('gulp')
  , browserify  = require('gulp-browserify')
  , purescript  = require('gulp-purescript');

require("mandragora-bucket")(gulp);

gulp.task('site', function() {
    return gulp.src(['site/Main.purs', 'src/**/*.purs', 'bower_components/purescript-*/src/**/*.purs']).pipe(purescript.psc({
        main: 'Site',
        modules: ['Site']
    })).pipe(browserify({})).pipe(gulp.dest('site'));
});
