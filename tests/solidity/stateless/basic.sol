// SPDX-License-Identifier: UNLICENSED
pragma abicoder v2;
pragma solidity =0.7.6;

contract Greeter {
    string public greeting;

    function greet(string memory _greeting) public {
        greeting = _greeting;
    }

    function gm() public {
        greeting = "gm";
    }
}

contract GreeterTestSetup {
    Greeter greeter;

    function greeting() public view returns (string memory) {
        return greeter.greeting();
    }

    function setUp() public {
        greeter = new Greeter();
    }
}

contract GreeterTest is GreeterTestSetup {
    function greet(string memory _greeting) public payable {
        greeter.greet(_greeting);
    }

    function testShrinking(uint256 x, uint256 y) public payable {
        require(x * y <= 100, "product greater than 100");
    }


    function testStringFuzz(string memory myGreeting) public payable {
        greeter.greet(myGreeting);
        require(keccak256(abi.encodePacked(greeter.greeting())) == keccak256(abi.encodePacked(myGreeting)), "not equal");
    }

    function testFuzzFixedArray(uint256[2] memory x) public payable {
        // filter out x[1] == 0 since it'll drain all our gas in the
        // division by zero from all future tests
        if (x[1] == 0) return;
        require(x[1] / x[1] == 0);
    }

    function testFuzzVariableArray(uint256[] memory x) public payable {
        require(x[0] == x[1]);
    }

    function testFuzzBytes1(bytes1 x) public payable {
        require(x == 0);
    }

    function testFuzzBytes14(bytes14 x) public payable {
        require(x == 0);
    }

    function testFuzzBytes32(bytes32 x) public payable {
        require(x == 0);
    }

    function testFuzzI256(int256 x) public payable {
        require(x >= 0);
    }

    struct Foo {
        Bar bar;
    }

    struct Bar {
        uint256 baz;
    }

    function testFuzzAbiCoderV2(Foo memory foo) public payable {
        require(foo.bar.baz < 5);
    }

    // check the positive case
    function testGreeting() public payable {
        greeter.greet("yo");
        require(keccak256(abi.encodePacked(greeter.greeting())) == keccak256(abi.encodePacked("yo")), "not equal");
    }
    function testGreeting(string memory message) public payable {
        greeter.greet(message);
        require(keccak256(abi.encodePacked(greeter.greeting())) == keccak256(abi.encodePacked(message)), "not equal");
    }
    function testGreeting(string memory message, string memory _message) public payable {
        greeter.greet(message);
        require(keccak256(abi.encodePacked(greeter.greeting())) == keccak256(abi.encodePacked(message)), "not equal");
    }

    // check the unhappy case
    function testFailGreeting() public {
        greeter.greet("yo");
        require(keccak256(abi.encodePacked(greeter.greeting())) == keccak256(abi.encodePacked("hi")), "not equal to `hi`");
    }

    function testIsolation() public {
        require(bytes(greeter.greeting()).length == 0);
    }
}
