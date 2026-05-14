// SPDX-License-Identifier: MIT

contract PropertyRepro {
    uint256 public counter;

    function inc() public {
        counter++;
    }

    function echidna_counter_is_zero() public view returns (bool) {
        return counter == 0;
    }
}
