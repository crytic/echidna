pragma solidity >=0.4.25 <0.6.0;

contract Bar {
	function test() public pure returns (uint) {}
}

contract Foo {
	Bar bar;
	uint isOne;

	constructor(address _bar) public {
		bar = Bar(_bar);
		//bar = new Bar();
		isOne = 0;
	}

	function callBar() public {
		isOne = bar.test();
	}

	function echidna_test() public view returns (bool) {
    bar.test();
		return isOne == 0;
	}
}
