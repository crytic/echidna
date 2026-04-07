// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

import "../src/Counter.sol";

contract CounterTest {
    using CounterLib for Counter;

    Counter public counter;

    function setUp() public {
        counter = new Counter();
    }

    // Test basic increment sequence - 3 calls
    function test_increment_sequence() public {
        counter.increment();
        counter.increment();
        counter.increment();
    }

    // Test set and increment - 3 calls
    function test_set_and_increment() public {
        counter.setValue(100);
        counter.increment();
        counter.increment();
    }

    // Test with specific values - 3 calls
    function test_set_multiple_values() public {
        counter.setMultiple(5, 10, true);
        counter.increment();
        counter.setValue(42);
    }

    // Test decrement after increment - 3 calls
    function test_increment_then_decrement() public {
        counter.increment();
        counter.increment();
        counter.decrement();
    }

    // Test reset flow - 4 calls
    function test_operations_then_reset() public {
        counter.setValue(999);
        counter.increment();
        counter.setMultiple(1, 2, false);
        counter.reset();
    }

    // Fuzz test example - 1 call (args may be empty if non-constant)
    function testFuzz_setValue(uint256 value) public {
        counter.setValue(value);
    }

    // Test with view/pure calls mixed in - should only extract 4 state-changing calls
    function test_with_view_and_pure_calls() public {
        counter.increment();
        uint256 c = counter.getCount();  // view - should NOT be extracted
        counter.setValue(c);
        uint256 sum = counter.add(1, 2);  // pure - should NOT be extracted
        counter.setValue(sum);
        counter.decrement();
    }

    // Test library function - should extract the underlying calls (2 increments)
    function test_library_double_increment() public {
        counter.doubleIncrement();
    }

    // Test library function with parameter - should extract setValue + increment
    function test_library_set_and_increment() public {
        counter.setAndIncrement(50);
    }
}
