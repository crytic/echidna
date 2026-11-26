// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

contract NativeInvariant {
    uint256 private counter;
    uint256 private unrelatedState;
    address private writer;
    address private origin;

    function advance(uint256 next) public {
        require(next == counter + 1 && next <= 2);
        counter = next;
        writer = msg.sender;
        origin = tx.origin;
    }

    // An expensive target that the experiment must not explore implicitly.
    function unrelated(uint256 n) public {
        for (uint256 i = 0; i < n; i++) {
            unrelatedState += i;
        }
    }

    function echidna_not_two() public view returns (bool) {
        return counter != 2;
    }

    function echidna_at_most_two() public view returns (bool) {
        return counter <= 2;
    }

    function echidna_revert_expected() public view returns (bool) {
        require(counter == 2);
        return true;
    }

    function echidna_context() public view returns (bool) {
        return (counter == 0 || writer == origin)
            && msg.sender == address(0xcafe) && tx.origin == address(0xcafe);
    }
}
