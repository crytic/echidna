// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

// Smartian's motivating example: f(x), g(72), h(), with x % 32 == 1.
contract Motiv {
    address owner;
    uint stateA;
    uint stateB;
    uint constant MODULUS = 32;

    constructor() public {
        owner = msg.sender;
    }

    function f(uint x) public {
        if (msg.sender == owner) stateA = x;
    }

    function g(uint y) public {
        if (stateA % MODULUS == 1) stateB = y - 10;
    }

    function h() public view {
        assert(stateB != 62);
    }
}
