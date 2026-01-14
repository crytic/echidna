// Test contract for verifying gas accounting with require(false)
contract TestGasRequire {
    event AssertionFailed(string message);

    // This function always reverts with require(false)
    function test_require() public {
        emit AssertionFailed("require failed");
        require(false, "Always reverts");
    }
}
