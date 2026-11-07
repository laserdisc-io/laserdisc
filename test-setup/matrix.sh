#!/usr/bin/env bash
# Runs the integration tests of one version's import against a server of that version, and a cluster of four of its nodes, in containers
# of their own that are removed afterwards. Set LASERDISC_PORT_OFFSET to move every port, so that it can run next to other servers.
#
#   test-setup/matrix.sh redis_7_4
set -euo pipefail

import="${1:?usage: test-setup/matrix.sh <import>, like redis_7_4 or valkey_9_0}"
offset="${LASERDISC_PORT_OFFSET:-0}"
version="$(echo "${import#*_}" | tr _ .)"

case "$import" in
  redis_*)  image="redis:$version-alpine";         server=redis-server;  cli=redis-cli;  suites=Redis;  variable=LASERDISC_REDIS_IMPORT;  port=6379; first=7100 ;;
  valkey_*) image="valkey/valkey:$version-alpine"; server=valkey-server; cli=valkey-cli; suites=Valkey; variable=LASERDISC_VALKEY_IMPORT; port=6381; first=7120 ;;
  *) echo "$import isn't a Redis or Valkey import" >&2; exit 1 ;;
esac

port=$((port + offset))
first=$((first + offset))
ports="$first $((first + 1)) $((first + 2)) $((first + 3))"
name="laserdisc-$import-$offset"

node() { echo "$server --port $1 --cluster-enabled yes --cluster-config-file nodes-$1.conf --protected-mode no"; }
nodes=""
published=""
for p in $ports; do
  published="$published -p $p:$p"
  if [ "$p" = "$((first + 3))" ]; then nodes="$nodes exec $(node "$p")"; else nodes="$nodes $(node "$p") --daemonize yes;"; fi
done

trap 'docker rm -f "$name" "$name-cluster" >/dev/null 2>&1 || true' EXIT
docker run -d --name "$name" -p "$port:6379" "$image" >/dev/null
# shellcheck disable=SC2086
docker run -d --name "$name-cluster" $published --entrypoint sh "$image" -c "$nodes" >/dev/null

for _ in $(seq 1 60); do
  ready=yes
  docker exec "$name" "$cli" ping >/dev/null 2>&1 || ready=no
  for p in $ports; do docker exec "$name-cluster" "$cli" -p "$p" ping >/dev/null 2>&1 || ready=no; done
  [ "$ready" = yes ] && break
  sleep 1
done
[ "$ready" = yes ] || { echo "$image didn't start" >&2; exit 1; }

echo "Testing $import against $image"
env "$variable=$import" "LASERDISC_PORT_OFFSET=$offset" sbt "++3.9.0" "integration/testOnly laserdisc.fs2.$suites*"
