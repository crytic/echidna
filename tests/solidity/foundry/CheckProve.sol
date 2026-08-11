// SPDX-License-Identifier: MIT

// "check" and "prove" functions are symbolic entry points, but they are also
// valid fuzzing targets: when symbolic execution is disabled, they are fuzzed
// like any other Foundry test function.
contract CheckProveTest {
    function IS_TEST() public pure returns (bool) {
        return true;
    }

    function check_small(uint256 x) public pure {
        assert(x < 10);
    }

    function prove_small(uint256 x) public pure {
        assert(x < 10);
    }
}
