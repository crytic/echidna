// Test contract for verifying gas accounting with AssertionFailed event
contract TestGasEvent {
    event AssertionFailed(string message);

    // This function emits AssertionFailed event
    function test_event() public {
        emit AssertionFailed("test failed");
    }
}
