#!/usr/bin/env ../test/libs/bats-core/bin/bats

load helpers

@test "Call gethostname (C)" {
    run ./test/signal_1
    assert_success
    assert_output $(uname -n)
}

@test "Register handler for interrupt and raise SIGINT" {
    run ./test/signal_2
    assert_success
    assert_output -p "HANDLER: SIGNAL INTERRUPT"
    assert_output -p "Raised signal"
    assert_output -p "Current PID"
}

@test "Call nanosleep (C)" {
    run ./test/signal_3
    assert_success
    refute_output -p "errno:"
}
