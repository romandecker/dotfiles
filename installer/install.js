"use strict";

var BPromise = require( "bluebird" );
var osenv = BPromise.promisifyAll( require( "osenv" ) );
var fs = BPromise.promisifyAll( require("fs-extra") );
var path = require( "path" );
var childProcess = require( "child_process" );
var confirm = require( "confirm-simple" );
var request = require( "request" );
require( "colors" );

var PLUG_URL = "https://raw.githubusercontent.com/" +
               "junegunn/vim-plug/master/plug.vim";

// all relative to $HOME
var FILES = [
    ".zshrc",
    { "dotgitignore": ".gitignore" },
    ".vimrc",
    ".vrapperrc",
    ".vim/ftplugin",
    ".tmux.conf",
    ".tmuxifier/layouts/development.window.sh",
    ".tmuxifier/templates/session.sh"
];

var repoFiles = {};

FILES.forEach( function(file) {

    var repoName, fsName;
    if( typeof file === "object" ) {
        repoName = Object.keys( file )[0];
        fsName = file[repoName];
    } else {
        repoName = fsName = file;
    }
    repoFiles[fsName] = path.resolve( path.join( __dirname, "..", repoName) );
} );

repoFiles[".tmuxifier/layouts/development.window.sh"] = path.join(
    __dirname,
    "..",
    ".tmuxifier",
    "layouts",
    "development.window.sh"
);

var installedFiles = {};

// all relative to $HOME
var REQUIRED_DIRECTORIES = [
    ".vim/undo",
    ".vim/backup",
    ".vim/tmp",
    ".vim/autoload",
    "bin"
];

var CHECK = "\u2713".green;
var CROSS = "\u2717".red;

var home;

var requiredDirs = {};

function exec( cmd, args, options ) {

    var process = childProcess.spawn( cmd, args, options );

    options = options || {};

    if( options.pipe ) {
        process.stdout.on( "data", function(data) {
            process.stdout.write( data );
        } );
        process.stderr.on( "data", function(data) {
            process.stderr.write( data );
        } );
    }

    return new BPromise( function(resolve, reject) {
        process.on( "error", reject );
        process.stdout.on( "error", reject );
        process.stderr.on( "error", reject );

        process.on( "exit", function(code) {
            if( code === 0 ) {
                resolve();
            } else {
                reject( cmd + " exited with code " + code );
            }
        } );
    } );
}

console.log( "Checking current system state..." );
osenv.homeAsync().then( function(h) {

    home = h;

    FILES.forEach( function(file) {
        if( typeof file === "object" ) {
            file = file[ Object.keys(file)[0] ];
        }
        installedFiles[file] = path.join( home, file );
    } );

    REQUIRED_DIRECTORIES.forEach( function(dir) {
        requiredDirs[dir] = path.join( home, dir );
    } );

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

        return exec( "git",
                ["clone",
                "https://github.com/olivierverdier/zsh-git-prompt.git"],
                { cwd: requiredDirs.bin, pipe: true } );

    }
} ).then( function() {
    var tmuxifier = path.join( home, ".tmuxifier" );
    if( fs.existsSync(tmuxifier) ) {
        console.log( ".tmuxifier".magenta, "exists", CHECK );
    } else {
        console.log( ".tmuxifier".magenta, "doesn't exist, cloning...".yellow );

        return exec( "git",
                            ["clone",
                             "https://github.com/jimeh/tmuxifier.git",
                             ".tmuxifier"],
                             { cwd: home, pipe: true } ).then( function() {

            console.log( "Removing session layout template file " +
                         "(it's replaced with a custom template)" );
            return fs.unlinkSync( path.join(home,
                                  ".tmuxifier",
                                  "templates",
                                  "session.sh"
            ) );

        } );
    }

} ).then( function() {
    var tpm = path.join( home, ".tmux", "plugins", "tpm" );
    if( fs.existsSync(tpm) ) {
        console.log( ".tmux/plugins/tpm".magenta, "exists", CHECK );
    } else {
        console.log( ".tmux/plugins/tpm".magenta, "doesn't exist, cloning...".yellow );

        return exec( "git",
                     ["clone",
                      "https://github.com/tmux-plugins/tpm",
                      tpm],
                     { cwd: home, pipe: true } );
    }

} ).then( function() {

    var toInstall = [];
    FILES.forEach( function(file) {

        if( typeof file === "object" ) {
            file = file[ Object.keys(file)[0] ];
        }

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
        return fs.symlinkAsync(
            repoFiles[file], installedFiles[file] ).then( function() {
                console.log( "Installing link to", file.magenta, "...", CHECK );
            } );

    } ) );
} ).then( function() {

    var plug = path.join( home, ".vim/autoload/plug.vim" );

    if( fs.existsSync(plug) ) {
        console.log( "plug".magenta, "exists", CHECK );
    } else {
        console.log( "plug".magenta, "doesn't exist, downloading...".yellow );

        var autoload = path.dirname(plug);
        return fs.mkdirsAsync( autoload ).then( function() {

            var plugFile = fs.createWriteStream( plug );

            request( PLUG_URL ).pipe( plugFile );
        } );
    }

} ).then( function() {

    console.log( "Making sure git knows about global .gitignore..." );

    return exec(
        "git",
        ["config", "--global",
         "core.excludesfile", "~/.gitignore"],
        { pipe: true }
    ).then( function() {
        console.log( "Configured", ".gitignore".magenta, CHECK );
    } );

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
} ).catch( function(err) {
    console.error( "Installation cancelled:".red.bold );
    console.error( err );
} );

