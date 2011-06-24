#!/bin/sh

history_file=${XDG_DATA_HOME:-$HOME/.local/share}/uzbl/history
[ -r "$history_file" ] || exit 1

wmctrl -xa emacs
goto=$(emacsclient -e "(anything-c-uzbl-history \"$history_file\")")
goto=${goto%\"*}
goto=${goto#\"*}
[ -n "$goto" ] && echo "uri $goto" > $4
#[ -n "$goto" ] && echo "uri $goto" | socat - unix-connect:$5
wmctrl -xa uzbl
