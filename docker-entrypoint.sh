#!/bin/sh
set -eu

seed_version=1
version_file=/data/.phase-db-seed-version

mkdir -p /data

if [ ! -f "$version_file" ] || [ "$(cat "$version_file")" != "$seed_version" ]; then
    if [ -f /usr/share/phasedb/phase.db ]; then
        cp /usr/share/phasedb/phase.db /data/phase.db
    fi
    printf '%s\n' "$seed_version" > "$version_file"
fi

exec /home/phasedb/phasedb-server "$@"
