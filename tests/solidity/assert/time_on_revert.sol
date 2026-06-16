// Regression test for the "time advances on revert" fix (PR #1554).
//
// Only EVM *state* is rolled back when a transaction reverts; the block clock
// must keep moving forward. `og` is the timestamp captured at deployment.
//
// With `maxTimeDelay: 1` and `seqLen: 2`, reaching `og + 2` needs two
// time-advancing steps, and the only path passes through `og + 1` -- where the
// require below reverts. So the assert can fail only if that reverting step
// still advanced time, which is exactly the behavior the fix restores.
// Without the fix the revert rolls time back and `og + 2` is unreachable.
contract C {
    uint256 og;

    constructor() {
        og = block.timestamp;
    }

    function f() public {
        require(block.timestamp != og + 1);
        assert(block.timestamp != og + 2);
    }
}
