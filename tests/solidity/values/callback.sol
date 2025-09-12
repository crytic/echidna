// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Test {
    bool internal callbackExecuted = false;

    // The function we want to encode and pass to the callback
    function transfer(address to, uint256 value) public {
        // Ensure that only the contract itself can call this function
        require(msg.sender == address(this), "transfer can only be called by the contract");
        callbackExecuted = true;
    }

    // The function that receives the encoded calldata
    function callback(bytes calldata data) public {
        address(this).call(data);
    }

    // The Echidna property to test the callback
    function echidna_callback_executed() public view returns (bool) {
        return !callbackExecuted;
    }
}
