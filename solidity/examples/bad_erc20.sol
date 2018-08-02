pragma solidity ^0.4.24;

contract NewCoin {
  
  mapping(address => uint256) internal balances;

  uint256 internal totalSupply_;

  function NewCoin() public {
    totalSupply_ = 300;
  }

  function totalSupply() public view returns (uint256) {
    return totalSupply_;
  }

  function transfer(address _to, uint _value) public returns (bool) {
    require(_to != address(0));
    require(_value <= balances[msg.sender]);

    uint256 balance = balances[msg.sender] - _value;
    uint256 toBalance = balances[_to] + _value;

    setBalances(msg.sender, balance, _to, toBalance);
  }

  function setBalances(address _firstAccount, uint256 _firstAmount,
        address _secondAccount, uint256 _secondAmount) internal {
        balances[_firstAccount] = _firstAmount;
        balances[_secondAccount] = _secondAmount;
  }
}

contract TEST is NewCoin {
  uint private initSupply;
  address private alice = 0x00a329c0648769a73afac7f9381e08fb43dbea50;
  address private bob = 0x00a329c0648769a73afac7f9381e08fb43dbea60;
  address private eve = 0x00a329c0648769a73afac7f9381e08fb43dbea70;

  function TEST() public {
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

