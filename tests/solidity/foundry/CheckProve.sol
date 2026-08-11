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

    // reverts are failures for test functions, and these are test functions too
    function check_revert(uint256 x) public pure {
        if (x >= 10) {
            revert("too large");
        }
    }
}

// In a stateful campaign, parameterless "check" and "prove" functions are
// checked after each transaction, like "invariant" functions
contract CheckProveStatefulTest {
    uint256 public counter;

    function IS_TEST() public pure returns (bool) {
        return true;
    }

    function increase(uint8 x) public {
        counter += x;
    }

    function check_counter() public view {
        assert(counter <= 5);
    }

    function prove_counter() public view {
        assert(counter <= 5);
    }
}
