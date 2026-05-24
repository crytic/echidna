// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

contract Target {
    uint256 public value;
    function set(uint256 x) external { value = x; }
    function inc() external { value += 1; }
}

contract InternalCallsTest {
    Target public target;

    function setUp() public {
        target = new Target();
    }

    // Direct call
    function test_direct() public {
        target.set(1);
    }

    // Simple internal call
    function test_internal() public {
        _helper();
    }

    function _helper() internal {
        target.set(2);
    }

    // Multiple calls to same internal
    function test_multi_internal() public {
        _helper_args(3);
        _helper_args(4);
    }

    function _helper_args(uint256 x) internal {
        target.set(x);
    }

    // Nested internal calls
    function test_nested_internal() public {
        _outer();
    }

    function _outer() internal {
        _inner();
        target.inc();
    }

    function _inner() internal {
        target.set(5);
    }

    // Internal call with constant parameter
    function test_internal_constant() public {
        _helper_args(42);
    }
}
