pragma solidity ^0.5.0;

contract FunWithNumbers {
    uint constant public tokensPerEth = 10;
    uint constant public weiPerEth = 1e18;
    mapping(address => uint) public balances;

    function buyTokens(uint256 msg_value) public payable {
        uint tokens = msg_value/weiPerEth*tokensPerEth; // convert wei to eth, then multiply by token rate
        balances[msg.sender] += tokens;
    }

    function sellTokens(uint tokens) public {
        require(balances[msg.sender] >= tokens);
        uint eth = tokens/tokensPerEth;
        balances[msg.sender] -= tokens;
        msg.sender.transfer(eth*weiPerEth); 
    }
}

contract VerifyFunWithNumbers is FunWithNumbers {
    
    uint contract_balance_old;
    
    constructor() public {
        contract_balance_old = address(this).balance;
    }
    
    event AssertionFailed(string message);
    
    modifier checkInvariants {
        uint sender_balance_old = balances[msg.sender];
        
        _;
        
        if (address(this).balance > contract_balance_old && balances[msg.sender] <= sender_balance_old) {
            emit AssertionFailed("Invariant violation: Sender token balance must increase when contract account balance increases");
            assert(false);
        }
        if (balances[msg.sender] > sender_balance_old && contract_balance_old >= address(this).balance) {
            emit AssertionFailed("Invariant violation: Contract account balance must increase when sender token balance increases");
            assert(false);
        }
        if (address(this).balance < contract_balance_old && balances[msg.sender] >= sender_balance_old) {
            emit AssertionFailed("Invariant violation: Sender token balance must decrease when contract account balance decreases");
            assert(false);
        }
        if (balances[msg.sender] < sender_balance_old && address(this).balance >= contract_balance_old) {
            emit AssertionFailed("Invariant violation: Contract account balance must decrease when sender token balance decreases");
            assert(false);
        }
       
        contract_balance_old = address(this).balance;
    }

    function buyTokens(uint256 msg_value) public payable checkInvariants {
        super.buyTokens(msg_value);
    }
            
    function sellTokens(uint tokens) public checkInvariants {
        super.sellTokens(tokens);
    }
}
