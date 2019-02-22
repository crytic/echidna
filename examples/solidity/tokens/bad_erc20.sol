contract NewCoin {
  
  mapping(address => uint256) internal balances;

  uint256 internal totalSupply_;

  constructor() public {
    totalSupply_ = 300;
  }

  function totalSupply() public view returns (uint256) {
    return totalSupply_;
  }

  function transfer(address _to, uint _value) public returns (bool) {
 
    require(_to != address(0));
    require(_value <= balances[msg.sender]);

    uint256 senderBalance = balances[msg.sender] - _value;
    uint256 receiverBalance = balances[_to] + _value;

    balances[msg.sender] = senderBalance;
    balances[_to] = receiverBalance;
  }
}

contract TEST is NewCoin {
  uint private initSupply;
  address private alice = 0x00A329C0648769a73aFAc7F9381E08fB43DbeA50;
  address private bob = 0x00a329C0648769a73AFaC7f9381E08Fb43dBeA60;
  address private eve = 0x00a329C0648769a73afAC7F9381e08fb43DBEA70;

  constructor() public {
    balances[alice] = 100;
    balances[bob] = 100;
    balances[eve] = 100;
    initSupply = totalSupply_;
  }

  function echidna_test() public returns (bool) {
    totalSupply_ = balances[alice] + balances[bob] + balances[eve];
    return (initSupply == totalSupply_);
  }
}

