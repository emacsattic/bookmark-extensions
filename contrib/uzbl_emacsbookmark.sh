#!/bin/sh

which zenity 2>&1 >/dev/null || exit 2
url=$6
# replace tabs, they are pointless in titles and we want to use tabs as delimiter.
title=$(echo "$7" | sed 's/\t/    /')

entry=`zenity --entry --text="Add Bookmark." --entry-text="$url $title\t"`
exitstatus=$?
if [ $exitstatus -ne 0 ]; then exit $exitstatus; fi

url=$(echo $entry | awk '{print $1}')

wmctrl -xa emacs

emacsclient -e "(bmkext-get-uzbl-bmk \"$title\" \"$url\")"
sleep 2

wmctrl -xa uzbl
