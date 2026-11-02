rowtype-test run, where it starts nothing: a run handed a server by
TEST_DATABASE_URL hands it on, and the command's status is the run's.

  $ export TEST_DATABASE_URL=postgres://test@127.0.0.1:1/test

Anything but run, a runtime it knows and a command is the usage:

  $ rowtype-test
  usage: rowtype-test run [--runtime podman|docker] -- COMMAND [ARGUMENT...]
  [2]
  $ rowtype-test run --runtime nope -- true
  usage: rowtype-test run [--runtime podman|docker] -- COMMAND [ARGUMENT...]
  [2]
  $ rowtype-test run --
  usage: rowtype-test run [--runtime podman|docker] -- COMMAND [ARGUMENT...]
  [2]

The server handed is the one every process of the run is handed:

  $ rowtype-test run -- sh -c 'echo "$TEST_DATABASE_URL"'
  postgres://test@127.0.0.1:1/test

The container a server handed runs in is handed on as it came:

  $ ROWTYPE_TEST_CONTAINER='podman rowtype-test-1-2' rowtype-test run -- sh -c 'echo "$ROWTYPE_TEST_CONTAINER"'
  podman rowtype-test-1-2

The run's status is its command's, a signal's as a shell gives it:

  $ rowtype-test run -- sh -c 'exit 3'
  [3]
  $ rowtype-test run -- sh -c 'kill -TERM $$'
  [143]
