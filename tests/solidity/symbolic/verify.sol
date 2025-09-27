contract C {
  function f() public returns (uint256) {
    return 42;
  }
}

contract VulnerableContract {
  C c = new C(); 
  mapping (uint256 => uint256) a;
  uint256 private last_number = block.number;
  function simple(uint256 x) public payable {
    a[12323] = ((x >> 5) / 7777);
    if (a[12323] == 2222) {
       assert(false); // BUG
    }
  }
  function array(uint256[3] calldata xs) public {
    assert(xs[0] != xs[2] + 1);
  }
  
  function negative(int24 x) public {
    assert(x != -42);
  }

  function correct(int24 x) public {
    assert(int256(x) != type(int256).max);
  }

  function close(uint256 x) public {
    assert(block.number != last_number + 123);
  }

  function far(uint256 x) public {
    assert(block.number != type(uint256).max);
  }

  function onlyDeployed(address x) public {
    uint256 y = C(x).f();
    assert(y != 42);
  }

}
