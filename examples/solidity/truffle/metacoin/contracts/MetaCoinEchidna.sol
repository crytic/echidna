pragma solidity >=0.4.25 <0.6.0;

import "./MetaCoin.sol";

contract TEST is MetaCoin {

	function mint(uint ammount) public {
		balances[msg.sender] = ammount;
	}

	function echidna_convert() public view returns (bool) {
		return getBalanceInEth(msg.sender) >= getBalance(msg.sender);
	}
}	
