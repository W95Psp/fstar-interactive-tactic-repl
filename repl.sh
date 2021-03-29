#! /usr/bin/env bash

CACHE_STDOUT=$(mktemp "/tmp/cache-stdoutXXXXX")
echo "$CACHE_STDOUT"

rm /tmp/fifo_in
rm /tmp/fifo_out
mkfifo /tmp/fifo_in
mkfifo /tmp/fifo_out

function stdout () {
    while true; do
	cat /tmp/fifo_out >> $CACHE_STDOUT
	redraw
    done
    # tail -f /tmp/interact_out
}
function stdin () {
    while read -e line; do
	echo "$line" >> /tmp/fifo_in
	echo "> $line" >> $CACHE_STDOUT
	redraw
    done
}
function redraw () {
    COLS=$(tput cols)
    LINES=$(tput lines)
    tput clear
    tail -n "$((LINES - 1))" "$CACHE_STDOUT"
    tput cup "$LINES" 2
    printf "λ "
}

# trap 'check_terminal_size' redraw

redraw
stdout &
stdin

wait

