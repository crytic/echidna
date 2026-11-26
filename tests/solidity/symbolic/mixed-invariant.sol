// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

contract MixedInvariant {
    uint256 private phase;

    function arm(uint256 key) public {
        require(phase == 0 && key == 41);
        phase = 1;
    }

    // Selecting arm(uint256) must not also select this overload.
    function arm(bytes32) public {}

    function trigger(uint256 key) public {
        require(phase == 1 && key == 42);
        phase = 2;
    }

    function shortcut_no_symexec(uint256) public { phase = 2; }
    function readView() public view returns (uint256) { return phase; }
    function readPure() public pure returns (uint256) { return 0; }

    function echidna_never_finished() public view returns (bool) {
        return phase != 2;
    }

    // A property's writes must never become a transition in another search.
    function echidna_mutator() public returns (bool) {
        phase = 2;
        return true;
    }
}
