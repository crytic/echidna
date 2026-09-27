pragma solidity ^0.8.0;

contract TypeProbe {
    uint256 public seen;
    event Seen(uint256 value);
    event SignedSeen(int256 value);
    event MixedSeen(uint8 first, uint8 second, uint256 amount, bytes32 value);

    function record(uint256 x) internal {
        seen = x;
        emit Seen(x);
    }

    function f_none() public { record(8000); }
    function f_uint256(uint256 x) public { record(x); }
    function f_uint8(uint8 x) public { record(x); }
    function f_uint32(uint32 x) public { record(x); }
    function f_int128(int128 x) public {
        seen = uint256(int256(x));
        emit SignedSeen(x);
    }
    function f_bytes4(bytes4 x) public { record(uint32(x)); }
    function f_bytes32(bytes32 x) public { record(uint256(x)); }
    function f_bytes32_mixed(uint8 first, uint8 second, uint256 amount, bytes32 x) public {
        seen = uint256(x);
        emit MixedSeen(first, second, amount, x);
    }
    function f_addr(address x) public { record(uint160(x)); }
    function f_bool(bool x) public { record(x ? 1 : 0); }
    function f_uint8s(uint8[2] memory xs) public {
        record(xs[0]);
        record(xs[1]);
    }
    function f_int128s(int128[] memory xs) public {
        for (uint256 i = 0; i < xs.length; i++) f_int128(xs[i]);
    }
    function overloaded(uint8 x) public { record(x); }
    function overloaded(uint32 x) public { record(x); }
}
