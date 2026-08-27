#!/usr/bin/env bash

set -euo pipefail

PKGCONFIG_DIR="/usr/lib64/pkgconfig"
COMPAT="$PKGCONFIG_DIR/sdl2-compat.pc"
LINK="$PKGCONFIG_DIR/sdl2.pc"

if [[ ! -f "$COMPAT" ]]; then
    echo "Error: $COMPAT not found."
    exit 1
fi

sudo ln -sf "$COMPAT" "$LINK"

echo "Created $LINK -> $COMPAT"
