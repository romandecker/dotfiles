"use strict";

var BPromise = require( "bluebird" );
var os = require( "os" );
var osenv = BPromise.promisifyAll( require( "osenv" ) );
var fs = BPromise.promisifyAll( require("fs-extra") );
var path = require( "path" );
var childProcess = require( "child_process" );
var confirm = require( "confirm-simple" );
var request = require( "request" );
require( "colors" );

var CHECK = "\u2713".green;
var CROSS = "\u2717".red;
var WARN = "\u26a0".yellow;

var PLUG_URL = "https://raw.githubusercontent.com/" +
               "junegunn/vim-plug/master/plug.vim";

var OH_MY_ZSH_URL = "git@github.com:robbyrussell/oh-my-zsh.git";
var TMUXIFIER_URL = "git@github.com:jimeh/tmuxifier.git";
var TPM_URL = "git@github.com:tmux-plugins/tpm.git";
var POWERLINE_FONTS_URL = "git@github.com:powerline/fonts.git";

var FONTS_DIR_DARWIN = "Library/Fonts";
var FONTS_DIR_LINUX = ".fonts";

// all relative to $HOME
var REQUIRED_DIRECTORIES = [
  ".vim/undo",
  ".vim/backup",
  ".vim/tmp",
  ".vim/autoload",
  ".dotfiles"
];

// all relative to $HOME
var FILES = [
  { "dotgitignore": ".gitignore" },
  ".vimrc",
  ".vimrc.abbreviations",
  ".nvimrc",
  ".zshrc",
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
  ".oh-my-zsh/custom/tmuxifier.zsh"
];

// files/directories that will be linked after plugin installation
var VIM_PLUGIN_FILES = [
  ".vim/UltiSnips"
];

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

} ).then(
  buildFilesToInstall.bind( null, FILES )
).then( function( toInstall ) {

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

} ).then(
  installFiles
).then( function() {

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

  var fontsDir;
  if( os.platform() === "darwin" ) {
    fontsDir = FONTS_DIR_DARWIN;
  } else {
    // linux
    
    fontsDir = FONTS_DIR_LINUX;
  }

  var sampleFile = path.join( home, fontsDir, "ter-powerline-x12b.pcf.gz" );

  if( fs.existsSync(sampleFile) ) {
    console.log( "Checking fonts...", CHECK );
    return;
  } else {
    console.log( "Checking fonts... installing" );
  }

  var cloneDir = path.join( os.tmpdir(), "fonts-" + new Date().getTime() );
  return clone(
    "Temporary font repo",
    POWERLINE_FONTS_URL,
    cloneDir
  ).then( function() {
    return exec(
      path.join( cloneDir, "install.sh" ),
      [],
      { stdio: "inherit" }
    );
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
      ["-c", "PlugInstall", "-c", "PlugUpdate", "-c", "qall"],
      { stdio: "inherit" }
    );
  } );
} ).then( function() {

  console.log( "Linking additional vim-plugin-specific files..." );
  var toInstall = buildFilesToInstall( VIM_PLUGIN_FILES );

  if( toInstall.length > 0 ) {
    console.log( "Linking " + toInstall.length + " additional files..." );

    return installFiles( toInstall );
  }

} ).then( function() {
  console.log( "Done!".green.bold );
  console.log();
  console.log( "What happened?".bold );
  console.log( "* ~/.zshrc is installed" );
  console.log( "* ~/.oh-my-zsh is installed" );
  console.log( " * put custom zsh-configs in ~/.oh-my-zsh/custom/" );
  console.log( "* ~/.vimrc is installed" );
  console.log( "* ~/.tmux.conf is installed" );
  console.log( "* ~/.tmuxifier is installed and sourced" );
  console.log();
  console.log( "What to do now?".bold );
  console.log( "* Make sure zsh is your default shell" );
  console.log( "* Make sure your terminal uses one of the patched fonts" );
  console.log( "* If you're on a Mac, make sure you have " +
                  "reattach-to-user-namespace installed!" );
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
       to],
      { cwd: home, pipe: true }
    ).return( true );
  }
}

/*
 * Takes in an array of relative paths/objects that each point to a file in this
 * repository and builds a dictionary that has a key for each of the entries.
 * The value for the key will be the fully qualified path to that file within
 * this repo.
 */
function buildRepoFiles( sourceArray ) {

  var repoFiles = {};
  sourceArray.forEach( function(file) {

    var repoName, fsName;
    if( typeof file === "object" ) {
      repoName = Object.keys( file )[0];
      fsName = file[repoName];
    } else {
      repoName = fsName = file;
    }
    repoFiles[fsName] = path.resolve( path.join( __dirname, "..", repoName) );
  } );

  return repoFiles;
}

/*
 * Takes in an array of relative paths/objects that each point to a file in this
 * repository and builds a dictionary that has a key for each of the entries.
 * The value for the key will be the fully qualified path to where that file
 * SHOULD reside after installation.
 */
function buildInstalledFiles( sourceArray, home ) {
  var installedFiles = {};

  sourceArray.forEach( function(file) {
    if( typeof file === "object" ) {
      file = file[ Object.keys(file)[0] ];
    }
    installedFiles[file] = path.join( home, file );
  } );

  return installedFiles;
}

/*
 * Takes in an array of relative paths/objects that each point to a file in this
 * repository and builds an array consisting of an entry
 *
 *  {
 *     src: "/absolute/path/to/repo.file",
 *     dest: "/absolute/installation/path.file"
 *  }
 *
 *  for each name. Additionally, an overview will be printed.
 */
function buildFilesToInstall( names ) {
  
  var repoFiles = buildRepoFiles( names );
  var installedFiles = buildInstalledFiles( names, home );

  var toInstall = [];
  names.forEach( function(file) {

    var name = file;
    if( typeof file === "object" ) {
      name = Object.keys(file)[0];
      file = file[name];
    }

    var installed = installedFiles[file];
    var repo = repoFiles[file];

    if( fs.existsSync(installed) ) {
      var stats = fs.lstatSync( installed );
      if( stats.isSymbolicLink() ) {
        var linked = fs.readlinkSync( installed );

        if( linked === repo ) {
          console.log( name.magenta,
                       "is already linked to this repo",
                       CHECK );
        } else {
          console.log( name.magenta,
                       "exists and is not linked to this repo", CROSS );
          console.log( " * It currently points to " + linked.blue );
          console.log( " * If you wish to install", name.magenta,
                       "as well, please remove it manually first" );
        }
      } else {
        console.log( name.magenta, "is not a symlink", WARN );
      }
    } else {
      console.log( name.magenta, "does not exist", CROSS );
      toInstall.push( {
        name: file,
        src: repoFiles[file],
        dest: installedFiles[file]
      } );
    }

  } );

  return toInstall;
}

function installFiles( toInstall ) {

  return BPromise.all( toInstall.map( function(file) {
    return fs.symlinkAsync( file.src, file.dest ).then( function() {
      console.log( "Installing link to", file.name.magenta, "...", CHECK );
    } );
  } ) );
}
