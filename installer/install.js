"use strict";

var BPromise = require( "bluebird" );
var osenv = BPromise.promisifyAll( require( "osenv" ) );
var fs = BPromise.promisifyAll( require("fs-extra") );
var path = require( "path" );
var childProcess = require( "child_process" );
var confirm = require( "confirm-simple" );
require( "colors" );

// all relative to $HOME
var FILES = [
    ".zshrc",
    ".vimrc",
    ".vim/ftplugin",
    ".tmux.conf"
];

var repoFiles = {};

FILES.forEach( function(file) {
    repoFiles[file] = path.resolve( path.join( __dirname, "..", file) );
} );

var installedFiles = {};

// all relative to $HOME
var REQUIRED_DIRECTORIES = [
    ".vim/undo",
    ".vim/backup",
    ".vim/backup",
    "bin"
];

var CHECK = "\u2713".green;
var CROSS = "\u2717".red;

var home;

var requiredDirs = {};

function execAndPipe( cmd, args, options ) {

    var process = childProcess.spawn( cmd, args, options );

    process.stdout.on( "data", function(data) {
        process.stdout.write( data );
    } );
    process.stderr.on( "data", function(data) {
        process.stderr.write( data );
    } );

    return new Promise( function(resolve, reject) {
            process.on( "exit", function(code) {
                if( code === 0 ) {
                    resolve();
                } else {
                    reject( "Couldn't clone zsh-git-prompt repo" );
                }
            } );
    } );
}

console.log( "Checking current system state..." );
osenv.homeAsync().then( function(h) {

    home = h;

    FILES.forEach( function(file) {
        installedFiles[file] = path.join( home, file );
    } );

    REQUIRED_DIRECTORIES.forEach( function(dir) {
        requiredDirs[dir] = path.join( home, dir );
    } );

} ).then( function() {

    var toInstall = [];
    FILES.forEach( function(file) {

        var installed = installedFiles[file];
        var repo = repoFiles[file];

        if( fs.existsSync(installed) ) {
            var stats = fs.lstatSync( installed );
            if( stats.isSymbolicLink() ) {
                var linked = fs.readlinkSync( installed );
                
                if( linked === repo ) {
                    console.log( file.magenta,
                                 "is already linked to this repo",
                                 CHECK );
                } else {
                    console.log( file.magenta,
                                 "exists and is not linked to this repo",
                                 "(" + linked + ")", CROSS );
                    console.log( " * If you wish to install", file.magenta,
                                 "as well, please remove it manually first" );
                }
            } else {
                console.log( file.magenta, "is not a symlink", CROSS );
            }
        } else {
            console.log( file.magenta, "does not exist", CROSS );
            toInstall.push( file );
        }
        
    } );

    return toInstall;
} ).then( function( toInstall ) {
    
    if( toInstall.length === 0 ) {
        return toInstall;
    }

    return new BPromise( function(resolve, reject) {
        confirm( toInstall.length + " dotfiles will be linked. " +
                   "Do you want to continue?", function(ok) {
            if( ok ) {
                resolve( toInstall );
            } else {
                reject( "Installation cancelled by user" );
            }
        } );
    } );

} ).then( function( toInstall ) {

    return BPromise.all( toInstall.map( function(file) {
        console.log( "Installing link to", file.magenta, "..." );
        return fs.symlinkAsync(
            repoFiles[file], installedFiles[file] ).then( function() {
                console.log( CHECK );
            } );
            
    } ) );

} ).then( function() {
    console.log( "Making sure all required directories exist..." );

    return BPromise.all( REQUIRED_DIRECTORIES.map( function(name) {
        var dir = requiredDirs[name];
        return fs.mkdirsAsync( dir ).then( function() {
            console.log( " *", name.magenta, CHECK );
        } );
    } ) );
} ).then( function() {
    var gitPrompt = path.join( requiredDirs.bin, "zsh-git-prompt" );
    if( fs.existsSync(gitPrompt) ) {
        console.log( "git-prompt-zsh".magenta, "exists", CHECK );
    } else {
        console.log( "git-prompt-zsh".magenta,
                     "doesn't exist, cloning...".yellow );

        return execAndPipe( "git",
                ["clone",
                "https://github.com/olivierverdier/zsh-git-prompt.git"],
                { cwd: requiredDirs.bin } );
        
    }
} ).then( function() {
    var tmuxifier = path.join( home, ".tmuxifier" );
    if( fs.existsSync(tmuxifier) ) {
        console.log( ".tmuxifier".magenta, "exists", CHECK );
    } else {
        console.log( ".tmuxifier".magenta, "doesn't exist, cloning...".yellow );

        return execAndPipe( "git",
            ["clone",
             "https://github.com/jimeh/tmuxifier.git",
             ".tmuxifier"],
             { cwd: home } );
    }

} ).then( function() {
    console.log( "Done!".green.bold );
    console.log();
    console.log( "What happened?".bold );
    console.log( "* .zshrc is installed" );
    console.log( "* .vimrc is installed" );
    console.log( "* .tmux.conf is installed" );
    console.log( "* tmuxifier is installed (in ~/.tmuxifier) and sourced" );
    console.log( "* ~/bin exists" );
    console.log( " * it contains git-prompt-zsh, which is sourced by .zshrc" );
    console.log();
    console.log( "What to do now?".bold );
    console.log( "* Make sure zsh is your default shell" );
    console.log( "* Open a new terminal or source ~/.zshrc" );
    console.log( "* Run :PluginInstall from inside vim" );
} ).catch( function(err) {
    console.error( "Installation cancelled:".red.bold );
    console.error( err );
} );

