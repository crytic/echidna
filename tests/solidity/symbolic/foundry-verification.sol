// SPDX-License-Identifier: MIT
pragma solidity >=0.6.9;

interface FoundryVerificationVm {
    function assume(bool) external;
}

contract FoundryBoundedVerification {
    bool public constant IS_TEST = true;
    uint256 public phase;

    function setUp() public {
        require(phase == 0, "setUp must only run once");
        phase = 1;
    }

    function arm(uint256 value) public {
        if (phase == 1 && value == 41) phase = 2;
    }

    function finish() public {
        if (phase == 2) phase = 3;
    }

    function invariant_notFinished() public view {
        require(phase != 3, "ordinary revert from an invariant");
    }

    function shortcut_no_symexec() public {
        phase = 3;
    }
}

contract FoundryMutatingVerification {
    bool public constant IS_TEST = true;
    uint256 private observations;

    // Successful checks must retain their writes in the explored sequence.
    function statefulFuzz_observations() public {
        observations++;
        require(observations < 3, "third observation");
    }
}

contract FoundryFailureVerification {
    bool public constant IS_TEST = true;
    FoundryVerificationVm private constant vm =
        FoundryVerificationVm(address(uint160(uint256(keccak256("hevm cheat code")))));

    function test_revert() public pure {
        revert("ordinary test failure");
    }

    function check_revert() public pure {
        revert("ordinary check failure");
    }

    function prove_revert() public pure {
        revert("ordinary proof failure");
    }

    function testFail_revert() public pure {
        revert("expected");
    }

    function testFail_sometimesReverts(uint256 value) public pure {
        require(value == 42, "expected");
    }

    function test_assume(uint256 value) public {
        vm.assume(value == 7);
        require(value == 7, "unreachable");
    }

    function testFail_assume() public {
        vm.assume(false);
    }

    function invariant_context() public payable {
        require(msg.value == 0 && msg.sender == address(0x10000), "wrong call context");
    }

    function handlerRevert(uint256) public pure {
        revert("ordinary handler revert");
    }
}

// Naming alone must not change native assertion semantics.
contract NativeRevertVerification {
    function test_revert() public pure {
        revert("not an assertion");
    }
}
