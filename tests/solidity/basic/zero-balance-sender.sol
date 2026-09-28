// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

// Regression test for https://github.com/crytic/echidna/issues/1556
//
// The fuzzing senders are configured with a zero balance (see
// zero-balance-sender.yaml). On any real chain a transaction carrying more
// value than its sender owns is rejected before execution, so reaching `pay`
// with `msg.value > msg.sender.balance` means the fuzzer produced an
// impossible transaction.
contract ZeroBalanceSender {
    event PayCalled(address sender, uint256 msgValue, uint256 senderBalance);

    function pay() public payable {
        emit PayCalled(msg.sender, msg.value, msg.sender.balance);
        assert(msg.value <= msg.sender.balance);
    }
}
