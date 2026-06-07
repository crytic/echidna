// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

// Adapted from Smartian's motivating example.
// Requires a 3-step chain: f(x) → g(y) → h()
// where x % 32 == 1 and y == 72.
contract Motiv {
    address owner;
    uint private stateA = 0;
    uint private stateB = 0;
    uint CONST = 32;

    constructor() public {
        owner = msg.sender;
    }

    function f(uint x) public {
        if (msg.sender == owner) { stateA = x; }
    }

    function g(uint y) public {
        if (stateA % CONST == 1) {
            stateB = y - 10;
        }
    }

    function h() public view {
        assert(stateB != 62);
    }
}
