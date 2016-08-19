"use strict";
var gulp = require('gulp');
var gulpconnect = require('gulp-connect');
var gulpopen = require('gulp-open');
var browserify = require('browserify');
var reactify = require('reactify');
var source = require('vinyl-source-stream');
var eslint = require('gulp-eslint');

var config = {
    port: 9005,
    devBaseUrl: 'http://localhost',
    paths:{        
        html:'./sources/*.html',
        js:'./sources/*.js',
        mainJs:'./sources/main.js',
        build:'./builds'
    }
};

gulp.task('connect',function(){
    gulpconnect.server({
        root:[config.paths.build],
        port: config.port,
        base: config.devBaseUrl,
        livereload: true
    });
});

gulp.task('open',['connect'],function(){
    gulp.src(config.paths.build + '/index.html')
        .pipe(gulpopen({uri: config.devBaseUrl + ':' + config.port + '/'}));
});

gulp.task('html', function(){
    gulp.src(config.paths.html)
        .pipe(gulp.dest(config.paths.build))
        .pipe(gulpconnect.reload());
});

gulp.task('js',function(){
    browserify(config.paths.mainJs)
        .transform(reactify)
        .bundle()
        .on('error', console.error.bind(console))
        .pipe(source('bundle.js'))
        .pipe(gulp.dest(config.paths.build + '/scripts'))
        .pipe(gulpconnect.reload());
});

gulp.task('js-lint',function(){
    return gulp.src(config.paths.js)
        .pipe(eslint({configFile:'eslint.config.json'}))
        .pipe(eslint.format());
});

gulp.task('watch', function(){
    gulp.watch(config.paths.html, ['html']);
    gulp.watch(config.paths.js, ['js-lint', 'js']);
});

gulp.task('default', ['html', 'js-lint', 'js', 'open', 'watch']);


