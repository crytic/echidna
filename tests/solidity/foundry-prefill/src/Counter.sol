// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

// Library for Counter operations - tests that library calls are followed
library CounterLib {
    function doubleIncrement(Counter counter) external {
        counter.increment();
        counter.increment();
    }

    function setAndIncrement(Counter counter, uint256 value) external {
        counter.setValue(value);
        counter.increment();
    }
}

contract Counter {
    uint256 public count;
    uint256 public lastValue;
    bool public flag;

    function increment() public {
        count += 1;
    }

    function decrement() public {
        require(count > 0, "Count is zero");
        count -= 1;
    }

    function setValue(uint256 _value) public {
        lastValue = _value;
    }

    function setMultiple(uint256 _a, uint256 _b, bool _flag) public {
        count = _a;
        lastValue = _b;
        flag = _flag;
    }

    function reset() public {
        count = 0;
        lastValue = 0;
        flag = false;
    }

    // View function - should NOT be extracted for fuzzing
    function getCount() public view returns (uint256) {
        return count;
    }

    // Pure function - should NOT be extracted for fuzzing
    function add(uint256 a, uint256 b) public pure returns (uint256) {
        return a + b;
    }

    // Echidna property test
    function echidna_count_positive() public view returns (bool) {
        return count >= 0;
    }
}
