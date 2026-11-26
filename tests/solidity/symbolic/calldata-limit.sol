pragma solidity >=0.6.9;

contract CalldataLimit {
    // 4100 bytes including the selector: must be an error, never a proof.
    function checkTooLarge(uint256[128] calldata) external pure {
        assert(false);
    }
}
