pragma solidity >=0.4.25 <0.6.0;

contract BarInterface {
	function test() public pure returns (uint);
}

contract Foo {
	BarInterface bar;
	uint isOne;

	constructor(address _bar) public {
		bar = BarInterface(_bar);
		isOne = 0;
	}

	function callBar() public {
		isOne = bar.test();
	}

	function echidna_test() public view returns (bool) {
		return isOne == 0;
	}
}
