interface Balancer {
  function balanceOf(address owner) external view returns (uint256 balance);
}

interface Compound {
  function mint() external payable;
  function balanceOf(address owner) external view returns (uint256 balance);
  function transfer(address recipient, uint256 amount) external returns (bool);
}

interface Hevm {
  function prank(address) external;
  function roll(uint) external;
}

contract TestRPC {
  address constant HEVM_ADDRESS = 0x7109709ECfa91a80626fF3989D68f67F5b1DD12D;
  Hevm hevm = Hevm(HEVM_ADDRESS);
  // BAL: https://etherscan.io/address/0xba100000625a3754423978a60c9317c58a424e3D#code
  // Balancer bal = Balancer(0xba100000625a3754423978a60c9317c58a424e3D);
  // COMP: https://etherscan.io/token/0x4ddc2d193948926d02f9b1fe9e1daa0718270ed5
  Compound comp = Compound(0x4Ddc2D193948926D02f9B1fE9e1daa0718270ED5);

  uint balance = 0;

  function bar() public payable {
    assert(comp.balanceOf(address(this)) == 0);
    hevm.roll(15947727);
    comp.mint{value: msg.value}();
    balance = comp.balanceOf(address(this));
  }

  // block: 16198552
  /*function echidna_fetch_balance() public returns (bool) {
    return balance != 27099516537379438397130892;
  }*/

  // block: 15947726 or latest
  function echidna_mint_works() public returns (bool) {
    return balance == 0;
  }
}
