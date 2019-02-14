#!/bin/sh

# Simple helper functions to quickly "remember" files that are not
# usually checked into source control. Utilizes a sync-service such as
# dropbox to store files.

# Usage:
# remember-setup --base ~/Dropbox/remember --identifier my-machine
# Configure the sync directory and the name of this machine

# remember <file>
# Will create a link to the given file in a synced folder

# forget <file>
# Remove the link to the given file from the synced folder

REMEMBER_CONFIG_DIR="$HOME/.config/remember"

REMEMBER_IDENTIFIER_FILE=$REMEMBER_CONFIG_DIR/identifier
REMEMBER_BASE_FILE=$REMEMBER_CONFIG_DIR/base

REMEMBER_BASE=$(cat $REMEMBER_BASE_FILE 2>/dev/null || echo "$HOME/Dropbox/remember")
REMEMBER_IDENTIFIER=$(cat $REMEMBER_IDENTIFIER_FILE 2>/dev/null || hostname)

function remember-setup() (
    usage() {
        echo "Usage: $0 [options]"
        echo "Options:"
        echo " -b/--base <base> Use <base> as the base sync directory. Defaults to ~/Dropbox"
    }

    while [ "$1" != "" ]; do
        case $1 in
            -b|--base)
                if [ -n "$2" ]; then
                    REMEMBER_BASE="$2"
                    shift 2
                    continue
                else
                    echo "ERROR: '--base/-b' requires a non-empty option argument."
                    usage
                    exit 1
                fi
                ;;

            -i|--identifier)
                if [ -n "$2" ]; then
                    REMEMBER_IDENTIFIER="$2"
                    shift 2
                    continue
                else
                    echo "ERROR: '--identifier/-i' requires a non-empty option argument."
                    usage
                    exit 1
                fi
                ;;


            -h|-\?|--help)
                usage
                exit
                ;;
            --)              # End of all options.
                shift
                break
                ;;
            -?*)
                echo "ERROR: Unknown option (ignored): $1"
                usage
                exit 1
                ;;
            *)               # Default case: If no more options then break out of the loop.
                break
        esac
        shift
    done


    mkdir -p $REMEMBER_CONFIG_DIR
    echo "$REMEMBER_BASE" > $REMEMBER_BASE_FILE
    echo "$REMEMBER_IDENTIFIER" > $REMEMBER_IDENTIFIER_FILE
)

function remember-ls {
    find $REMEMBER_BASE -type l \
        | xargs -L 1 ls -la \
        | awk '{ for(i=1;i<=8;i++) $i=""; print }' \
        | sed "s# *$REMEMBER_BASE#\$REMEMBER_BASE#"
}

function remember-info {
    echo "Identifier: $REMEMBER_IDENTIFIER"
    echo "Base dir: $REMEMBER_BASE"

    echo
    echo "Linked files:"
    remember-ls
}

function remember {
    SOURCE_PATH=$(realpath $1)
    RELPATH=$(realpath --relative-to "$HOME" $SOURCE_PATH)
    SYNC_PATH=$REMEMBER_BASE/$REMEMBER_IDENTIFIER/$RELPATH

    DIR_PATH=$(dirname $SYNC_PATH)
    mkdir -p $DIR_PATH
    echo ln -s $SOURCE_PATH $SYNC_PATH
    ln -s $SOURCE_PATH $SYNC_PATH
}

function forget {
    SOURCE_PATH=$(realpath $1)
    RELPATH=$(realpath --relative-to "$HOME" $SOURCE_PATH)
    SYNC_PATH=$REMEMBER_BASE/$REMEMBER_IDENTIFIER/$RELPATH

    echo rm -f $SYNC_PATH
    rm -f $SYNC_PATH
}
