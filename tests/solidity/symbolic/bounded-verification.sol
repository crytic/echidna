// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

contract BoundedVerification {
    uint256 private phase;

    function arm(uint256 key) public {
        require(phase == 0 && key == 41);
        phase = 1;
    }

    // Requires two transactions, with independently solved arguments.
    function trigger(uint256 key) public view {
        require(key == 42);
        assert(phase != 1);
    }

    function prepare() public {
        require(phase == 1);
        phase = 2;
    }

    // Requires three transactions, including a no-argument setup call.
    function checkFinished(uint256 key) public view {
        require(key == 43);
        assert(phase != 2);
    }

    // Must not provide a shortcut, even in verification mode.
    function shortcut_no_symexec() public {
        phase = 2;
    }
}
