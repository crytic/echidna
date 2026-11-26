pragma solidity >=0.6.9;

contract CalldataLimit {
    // 4100 bytes including the selector: must be an error, never a proof.
    function checkTooLarge(uint256[128] calldata) external pure {
        assert(false);
    }

    // Even an empty dynamic argument needs an offset and a length word.
    function checkDynamicPrefix(uint256[127] calldata, bytes calldata) external pure {
        assert(false);
    }

    // 4068 bytes: the failing assertion must still be reachable.
    function checkBelowLimit(uint256[127] calldata) external pure {
        assert(false);
    }

    function checkSmall(uint256 value) external pure {
        assert(value == value);
    }
}
