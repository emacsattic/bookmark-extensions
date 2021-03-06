#!/bin/bash
## Title:
## Description: Switch to emacs-bookmarks list. 
## Author:Thierry Volpiatto<thierry dot volpiatto FROM gmail DOT com>
## Commentary: 

wmctrl -xa emacs

bookmark=$(emacsclient -e "(anything-w3m-bookmarks-from-uzbl)")
bookmark=${bookmark%\"*}
bookmark=${bookmark#\"*}

#[ -n "$bookmark" ] && echo "uri $bookmark" | socat - UNIX-CONNECT:$5
[ -n "$bookmark" ] && echo "uri $bookmark" > $4

wmctrl -xa uzbl

