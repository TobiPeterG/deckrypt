#!/bin/bash

check() {
    return 0
}

depends() {
    echo "base"
}

kmod_ins() {
    modinfo -k "$kernel" "$1" &>/dev/null
}

inst_conf() {
if [ -d "$1" ] && [ ! -z "$( ls -A "$1" )" ]; then
        inst "$1"
        inst_multiple $(find "$1" -type f)
    fi
}

installkernel() {
    hostonly="" instmods uinput
    
    for i in xone xpad-noone xpad; do
        ! kmod_ins "$i" || hostonly="" instmods "$i"
    done
}

install() {
    inst "/usr/bin/deckrypt"

    inst_simple "$moddir/deckrypt.service" "$systemdsystemunitdir/deckrypt.service"
    $SYSTEMCTL -q --root "$initdir" enable "deckrypt.service"
    inst_conf "/etc/deckrypt"
    inst_conf "/usr/share/deckrypt"
}
