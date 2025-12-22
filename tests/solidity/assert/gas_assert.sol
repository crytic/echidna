// Test contract for verifying gas accounting with assert(false)
contract TestGasAssert {
    // This function always reverts with assert(false)
    function test_assert() public {
        assert(false);
    }
}
