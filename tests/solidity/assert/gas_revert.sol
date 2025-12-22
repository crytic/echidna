// Test contract for verifying gas accounting with plain revert()
contract TestGasRevert {
    event AssertionFailed(string message);

    // This function just reverts with no message
    function test_revert() public {
        emit AssertionFailed("revert test");
        revert();
    }
}
