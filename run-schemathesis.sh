#! /bin/bash

SCHEMATHESIS="$(nix-build --no-out-link -A schemathesis)/bin/schemathesis"

./start-mock-api.sh &
sleep 3
server_pid=$!

# Define a function to stop the server when the script exits
function cleanup {
    echo "Stopping server..."
    kill $server_pid
}

# Run the test suite, with cleanup function executed on exit
trap cleanup EXIT

export PYTHONPATH="$(dirname "$(readlink -f "$0")")"
export SCHEMATHESIS_HOOKS=schemathesis-hooks

"$SCHEMATHESIS" run --checks all swagger.yaml --base-url http://localhost:8080