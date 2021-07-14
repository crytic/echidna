// Original example from https://github.com/b-mueller/sabre#example-2-integer-precision-bug

contract FunWithNumbers {
    uint constant public tokensPerEth = 10;
    uint constant public weiPerEth = 1e18;
    mapping(address => uint) public balances;

    function buyTokens() public payable {
        uint tokens = msg.value/weiPerEth*tokensPerEth; // convert wei to eth, then multiply by token rate
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
        }
        if (balances[msg.sender] > sender_balance_old && contract_balance_old >= address(this).balance) {
            emit AssertionFailed("Invariant violation: Contract account balance must increase when sender token balance increases");
        }
        if (address(this).balance < contract_balance_old && balances[msg.sender] >= sender_balance_old) {
            emit AssertionFailed("Invariant violation: Sender token balance must decrease when contract account balance decreases");
        }
        if (balances[msg.sender] < sender_balance_old && address(this).balance >= contract_balance_old) {
            emit AssertionFailed("Invariant violation: Contract account balance must decrease when sender token balance decreases");
        }
        
        contract_balance_old = address(this).balance;
    }

    function buyTokens() public payable checkInvariants {
        super.buyTokens();
    }
            
    function sellTokens(uint tokens) public checkInvariants {
        super.sellTokens(tokens);
    }
}
