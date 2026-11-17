#!/bin/sh
set -eu

seed_version=1
version_file=/data/.phase-db-seed-version

if [ ! -f "$version_file" ] || [ "$(cat "$version_file")" != "$seed_version" ]; then
    cp /usr/share/phasedb/phase.db /data/phase.db
    printf '%s\n' "$seed_version" > "$version_file"
fi

exec /home/phasedb/phasedb-server "$@"
