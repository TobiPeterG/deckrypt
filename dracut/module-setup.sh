#!/bin/bash

check() {
    return 0
}

depends() {
    echo "base"
}

installkernel() {
    hostonly="" instmods uinput xpad-noone
}

install() {
    # Include the deckrypt binary
    inst /usr/lib/dracut/modules.d/90deckrypt/deckrypt /usr/bin/deckrypt

    inst_simple "$moddir/deckrypt.service" "$systemdsystemunitdir/deckrypt.service"
    $SYSTEMCTL -q --root "$initdir" enable deckrypt.service
    if [ -d "/etc/deckrypt" ]; then
        inst /etc/deckrypt /etc/deckrypt
        inst_multiple $(find /etc/deckrypt -type f)
    fi
    if [ -d "/usr/share/deckrypt" ]; then
        inst /usr/share/deckrypt /usr/share/deckrypt
        inst_multiple $(find /usr/share/deckrypt -type f)
    fi
}
