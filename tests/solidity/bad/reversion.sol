pragma solidity ^0.8.0;

interface Target {
    function crash(
        address _creator,
        address _quoteToken,
        address _underlyingToken,
        uint256 _r0,
        uint256 _r_star,
        uint256 _k0,
        uint256 _cashIn,
        uint256 _initLPShares,
        address _vault
    ) external;
}

contract Crash {
    address constant target = address(0x1111);
    function crash(
        address _creator,
        address _quoteToken,
        address _underlyingToken,
        uint256 _r0,
        uint256 _r_star,
        uint256 _k0,
        uint256 _cashIn,
        uint256 _initLPShares,
        address _vault
    ) public {
        Target(target).crash(_creator, _quoteToken, _underlyingToken, _r0, _r_star, _k0, _cashIn, _initLPShares, _vault);
    }
}
