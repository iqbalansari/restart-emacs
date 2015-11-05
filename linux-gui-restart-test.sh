#!/bin/sh

# This scripts tests that Emacs restarts successfully under X
# It requires the following utilities installed
# xprop    - Available in x11-utils package on Ubuntu
# xwininfo - Available in x11-utils package on Ubuntu
# xdotool  - Available in xdotool package on Ubuntu

if [ -z "$DISPLAY" ] ; then
    echo "This script works only under X!"
    echo "Exiting ..."
    exit 2
fi

if [ -z $(which xprop 2> /dev/null)  ] || [ -z $(which xwininfo 2> /dev/null)  ] || [ -z $(which xdotool 2> /dev/null)  ] ; then
    echo "This script requires xprop, xwininfo and xdotool to work."
    echo "If you are on Ubuntu, please install x11-utils and xdotools packages to get these tools."
    echo "Exiting ..."
    exit 2
fi


poll_emacs_started () {
    for i in $(seq 1 10) ; do
        echo "Checking if Emacs started ... "

        if xwininfo -name $EMACS_WIN_NAME > /dev/null 2>&1 ; then
            echo "Emacs has started ... "
            return 0

        elif [ $i = 10 ] ; then
            echo "Giving up after 10 tries ... "
            return 1

        fi

        echo "Emacs hasn't started yet! Will try again in 5 seconds ... "
        sleep 5
    done

}

# So that user config does not interfere with functioning of test Emacs
HOME=/tmp

SUCCESS=1

# Name that should be given Emacs window, makes easier to search for window
EMACS_WIN_NAME=restart-emacs-test

# Setup things so that Emacs window is renamed appropriately after starting
mkdir -p /tmp/.emacs.d/
printf "(modify-frame-parameters nil (list (cons 'name \"$EMACS_WIN_NAME\")))" > /tmp/.emacs.d/init.el

# Launch Emacs
cask exec emacs -l restart-emacs.el > /dev/null 2>&1 &

if poll_emacs_started ; then
    EMACS_WINDOW_ID=$(xwininfo -name $EMACS_WIN_NAME | grep 'Window id:' | awk -F' ' '{print $4}')
    ORIG_EMACS_PID=$(xprop -name $EMACS_WIN_NAME | grep PID | awk -F' = ' '{print $2}')

    echo "Found Emacs running with window id $EMACS_WINDOW_ID and pid $ORIG_EMACS_PID .. "

    sleep 1

    echo "Asking Emacs to restart .. "
    xdotool search --name $EMACS_WIN_NAME key alt+x
    sleep 1
    xdotool search --name $EMACS_WIN_NAME type 'restart-emacs'
    sleep 1
    xdotool search --name $EMACS_WIN_NAME key Return


    sleep 5

    if poll_emacs_started ; then
        RESTARTED_EMACS_PID=$(xprop -name $EMACS_WIN_NAME | grep PID | awk -F' = ' '{print $2}')

        if [ $ORIG_EMACS_PID != $RESTARTED_EMACS_PID ] ; then
            echo "Successfully restarted Emacs!"
            echo "Test PASSED"
            SUCCESS=0
        else
            echo "The original Emacs was not killed properly!"
            echo "Test FAILED"
        fi

    else
        echo "Could not restart Emacs, after killing original one!"
        echo "Test FAILED"
    fi

else
    echo "Could start Emacs, please check for errros and try again!"
    SUCCESS=2
fi

# Cleanup
xdotool search --name $EMACS_WIN_NAME key ctrl+x
xdotool search --name $EMACS_WIN_NAME key ctrl+c
exit $SUCCESS
