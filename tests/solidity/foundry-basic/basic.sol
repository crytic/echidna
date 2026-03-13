// SPDX-License-Identifier: UNLICENSED
pragma abicoder v2;
pragma solidity >=0.7.5;

// Minimal Vm interface (this contract doesn't use forge-std).
interface Vm {
    function assume(bool condition) external pure;
}

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
    bool public IS_TEST = true;
    Greeter greeter;

    function greeting() public view returns (string memory) {
        return greeter.greeting();
    }

    function setUp() public {
        greeter = new Greeter();
    }
}

contract GreeterTest is GreeterTestSetup {
    function greet(string memory _greeting) public {
        greeter.greet(_greeting);
    }

    function testShrinking(uint256 x, uint256 y) public {
        require(x * y <= 100, "product greater than 100");
    }

    function testStringFuzz(string memory myGreeting) public {
        greeter.greet(myGreeting);
        require(keccak256(abi.encodePacked(greeter.greeting())) == keccak256(abi.encodePacked(myGreeting)), "not equal");
    }

    function testFuzzFixedArray(uint256[2] memory x) public {
        // filter out x[1] == 0 since it'll drain all our gas in the
        // division by zero from all future tests
        if (x[1] == 0) return;
        require(x[1] / x[1] == 0);
    }

    function testFuzzVariableArray(uint256[] memory x) public {
        require(x[0] == x[1]);
    }

    function testFuzzBytes1(bytes1 x) public {
        require(x == 0);
    }

    function testFuzzBytes14(bytes14 x) public {
        require(x == 0);
    }

    function testFuzzBytes32(bytes32 x) public {
        require(x == 0);
    }

    function testFuzzI256(int256 x) public {
        require(x >= 0);
    }

    struct Foo {
        Bar bar;
    }

    struct Bar {
        uint256 baz;
    }

    function testFuzzAbiCoderV2(Foo memory foo) public {
        require(foo.bar.baz < 5);
    }

    function testGreeting(string memory message) public {
        greeter.greet(message);
        require(keccak256(abi.encodePacked(greeter.greeting())) == keccak256(abi.encodePacked(message)), "not equal");
    }

    function testFuzzAssume(uint256 x) public {
        // keccak256("hevm cheat code") - standard hevm cheatcode address
        // https://github.com/argotorg/hevm/blob/ed90053fa0ed69e658a75ab0ed64d467f5a5448d/src/EVM.hs#L1861
        Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D).assume(x <= 100);
        assert(x <= 100);
    }
}
