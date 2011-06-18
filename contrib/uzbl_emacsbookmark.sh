#!/bin/sh
## Title:
## Description: Record a bookmark in emacs-bookmarks from uzbl. 
## Author:Thierry Volpiatto<thierry dot volpiatto FROM gmail DOT com>
## Commentary:

url="$6"
title="$7"

wmctrl -xa emacs

emacsclient -e "(bmkext-get-uzbl-bmk \"$title\" \"$url\")"
sleep 2

wmctrl -xa uzbl
