// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract PropertyRepro {
    uint256 public counter;

    function inc() public {
        counter++;
    }

    function echidna_counter_is_zero() public view returns (bool) {
        return counter == 0;
    }
}
