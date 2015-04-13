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

var OH_MY_ZSH_URL = "git@github.com:robbyrussell/oh-my-zsh.git";
var TMUXIFIER_URL = "git@github.com:jimeh/tmuxifier.git";
var TPM_URL = "git@github.com:tmux-plugins/tpm";

// all relative to $HOME
var FILES = [
    { "dotgitignore": ".gitignore" },
    ".vimrc",
    ".vrapperrc",
    ".vim/ftplugin",
    ".tmux.conf",
    ".tmux.conf.macosx",
    ".tmuxifier/layouts/development.window.sh",
    ".tmuxifier/templates/session.sh",
    ".oh-my-zsh/custom/aliases.zsh",
    ".oh-my-zsh/custom/bindings.zsh",
    ".oh-my-zsh/custom/env.zsh",
    ".oh-my-zsh/custom/functions.zsh",
    ".oh-my-zsh/custom/history.zsh",
    ".oh-my-zsh/custom/path.zsh",
    ".oh-my-zsh/custom/prompt.zsh",
    ".oh-my-zsh/custom/tmuxifier.zsh",
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
    ".dotfiles"
];

var CHECK = "\u2713".green;
var CROSS = "\u2717".red;
var WARN = "\u26a0".yellow;

var home;

var requiredDirs = {};

console.log( "Checking current system state..." );
osenv.homeAsync().then( function(h) {

  home = h;

  if( __dirname.indexOf(home) !== 0 ) {
    console.log( "Checking repo location...", CROSS );
    console.log( "Please clone this repo to ~/.dotfiles!" );
    throw new Error( "Repository not in the correct location!" );
  } else {
    console.log( "Checking repo location...", CHECK );
  }

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

  var ohMyZshPath = path.join( home, ".oh-my-zsh" );

  return clone(
    ".oh-my-zsh",
    OH_MY_ZSH_URL,
    ohMyZshPath
  );

} ).then( function() {
  var tmuxifierPath = path.join( home, ".tmuxifier" );

  return clone(
    ".tmuxifier",
    TMUXIFIER_URL,
    tmuxifierPath
  ).then( function( cloned ) {

    if( cloned ) {
      console.log( " * removing tmuxifier's session template" );

      return fs.unlinkSync(
        path.join(
          home,
          ".tmuxifier",
          "templates",
          "session.sh"
        )
      );
    }
  } );

} ).then( function() {
  var tpmPath = path.join( home, ".tmux", "plugins", "tpm" );

  return clone(
    ".tmux/plugins/tpm",
    TPM_URL,
    tpmPath
  );

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
        console.log( file.magenta, "is not a symlink", WARN );
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

  console.log( "Cleaning up unused plugins..." );

  return BPromise.delay( 1000 ).then( function() {
    return exec(
      "vim",
      ["-c", "PlugClean!", "-c", "qall"],
      { stdio: "inherit" }
    );
  } );

} ).then( function() {

  console.log( "Updating plugins..." );

  return BPromise.delay( 1000 ).then( function() {
    return exec(
      "vim",
      ["-c", "PlugInstall", "-c", "qall"],
      { stdio: "inherit" }
    );
  } );

} ).then( function() {
  console.log( "Done!".green.bold );
  console.log();
  console.log( "What happened?".bold );
  console.log( "* ~/.oh-my-zsh is installed" );
  console.log( " * put custom zsh-configs in ~/.oh-my-zsh/custom/" );
  console.log( "* ~/.vimrc is installed" );
  console.log( "* ~/.tmux.conf is installed" );
  console.log( "* ~/.tmuxifier is installed and sourced" );
  console.log();
  console.log( "What to do now?".bold );
  console.log( "* Make sure zsh is your default shell" );
  console.log( "* Open a new terminal or source ~/.zshrc" );
} ).catch( function(err) {
  console.error( "Installation cancelled:".red.bold );
  console.error( err );
} );

function exec( cmd, args, options ) {

  var child = childProcess.spawn( cmd, args, options );

  options = options || {};

  if( options.pipe ) {
    child.stdout.on( "data", function(data) {
      process.stdout.write( data );
    } );
    child.stderr.on( "data", function(data) {
      process.stderr.write( data );
    } );
  }

  return new BPromise( function(resolve, reject) {
    child.on( "error", reject );
    if( options.stdio !== "inherit" ) {
      child.stdout.on( "error", reject );
      child.stderr.on( "error", reject );
    }

    child.on( "exit", function(code) {
      if( code === 0 ) {
        resolve();
      } else {
        reject( cmd + " " + args.join( " " ) +
                " exited with code " + code );
      }
    } );
  } );
}

function clone( name, repo, to ) {

  if( fs.existsSync(to) ) {
    console.log( name.magenta, "exists", CHECK );
    return BPromise.resolve( false );
  } else {
    console.log( name.magenta, "doesn't exist, cloning...".yellow );

    return exec(
      "git",
      ["clone",
       repo,
       "~/.tmuxifier"],
      { cwd: home, pipe: true }
    ).return( true );
  }
}

