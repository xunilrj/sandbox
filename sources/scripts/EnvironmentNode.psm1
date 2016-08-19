function Test-Component($Name, $Package)
{
    if([System.String]::IsNullOrEmpty($Package)){
        $Package = $Name
    }

    Write-Verbose "Searching $Package"
    $result = command $Name
    if($result -eq $null){
        Write-Verbose "$Package not founded. Installing..."
        if($Name.ToLower() -eq "choco"){
            iwr https://chocolatey.org/install.ps1 -UseBasicParsing | iex
        }else{            
            choco install $Package -fvy *>&1 | % {Write-Verbose $_}
        }
    }else{
        Write-Verbose "$Package founded. Updating..."
        choco upgrade $Package -vy *>&1 | % {Write-Verbose $_}
    }

    refreshenv
}

function Start-NodeEnvironment
{
    $r = gci *.* -Recurse | Measure-Object
    
    if($r.Count -gt 0){
        Write-Error "Directory must be empty"
        return        
    }

    Test-Component choco chocolatey
    Test-Component node nodejs
    Test-Component npm
    
    Start-Npm
    Install-NPMPackage gulp,gulp-connect,gulp-open,vinyl-source-stream -Development    
    Install-NPMPackage gulp-eslint -Development
    Install-NPMPackage browserify,reactify
    Install-NPMPackage react,react-dom,react-router,flux
    

    mkdir sources\components
    mkdir .\sources\libs
    mkdir builds
        
    """use strict"";
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
        js:'./sources/**/*.js',
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

" | Out-File .\gulpfile.js -Encoding utf8 -Force

"<!DOCTYPE html>
<html>
<head>    
</head>
<body>
    <div class=""react-element"" data-react-element=""App""></div>  
    <div id=""asd"">
    </div>
    <button onclick=""javascript:var e = document.createElement('div');e.className='react-element';e.dataset.reactElement='App';document.getElementById('asd').appendChild(e);"">add</button>
    <script src=""scripts\bundle.js""></script>
</body>
</html>" | Out-File .\sources\index.html -Encoding utf8 -Force

'{
    "parserOptions":{
        "ecmaFeatures": {
            "jsx": true
        }
    },
    "env": {
        "browser": true,
        "node": true,
        "jquery": true
    },
    "rules": {
        "semi":"error"
    },
    "globals": {
        "jQuery": true,
        "$": true
    }
}' | Out-File .\eslint.config.json -Encoding utf8 -Force

"var React = require('react');
var ReactDOM = require('react-dom');
var app = require('./components/app.js');
ReactDOM.render(<app/>,document.getElementById('appcontainer'));
module.exports = {};" | Out-File .\sources\main.js -Encoding utf8 -Force

"""use strict"";
var React = require('react');
var App = React.createClass({
   render:function(){
        return (<div className=""title"">Working!</div>);
   }
});
module.exports = App;" | Out-File .\sources\components\app.js -Encoding utf8 -Force

"'use strict';
var listeners = [],
MutationObserver = window.MutationObserver || window.WebKitMutationObserver,
observer;
var check = function(){    
    for(var i = 0, len = listeners.length, listener, elements; i < len; i++){
        listener = listeners[i];        
        elements = document.querySelectorAll(listener.selector);
        for(var j = 0, jLen = elements.length, element; j < jLen; j++){
            element = elements[j];            
            if(!element.ready){
                element.ready = true;                
                listener.fn.call(element, element);
            }
        }
    }
};

module.exports = {
    run: function (selector, fn){        
        listeners.push({
            selector: selector,
            fn: fn
        });
        if(!observer){            
            observer = new MutationObserver(check);
            observer.observe(document.documentElement, {
                childList: true,
                subtree: true
            });
        }        
        check();
    }
}; " | Out-File .\sources\libs\ready.js -Encoding utf8 -Force
}

gci . -Recurse -Directory | ri -Force -Recurse -Verbose
gci . -Recurse -File | ri -Force -Recurse -Verbose
Start-NodeEnvironment
code .