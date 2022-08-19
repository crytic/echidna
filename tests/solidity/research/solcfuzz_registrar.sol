pragma solidity ^0.4.25;

// Name Registrar
contract NameRegistry {
    
    address public owner;
    bool public lock;
    
    struct NameRecord { // map hashes to addresses
        bytes32 name; // 
        address mappedAddress;
    }

    mapping(address => NameRecord) public registeredNameRecord; // records who registered names 
    mapping(bytes32 => address) public resolve; // resolves hashes to addresses
    
    modifier onlyOwner {
        require(msg.sender == owner);
        _;
    }
    
    constructor() public {
        owner = msg.sender;
    }
    
    function register(bytes32 _name, address _mappedAddress) public {
        // set up the new NameRecord
        NameRecord newRecord;
        newRecord.name = _name;
        newRecord.mappedAddress = _mappedAddress;
        resolve[_name] = _mappedAddress;
        registeredNameRecord[msg.sender] = newRecord;
    }

    function lock() onlyOwner public {
        lock = true;
    }
}

contract VerifyNameRegistry is NameRegistry {
    
    event AssertionFailed(string message);
    address public original_owner;

    constructor() {
        original_owner = owner;
    }

    modifier checkInvariants {

        bool lock_old = lock;

        _;

        if (msg.sender != original_owner && lock != lock_old) {
            emit AssertionFailed("[CONTRACT INVARIANT] Lock state variable was modified but sender is not the contract owner");
            assert(false);
        }
    }
    
    function register(bytes32 _name, address _mappedAddress) checkInvariants public {
        super.register(_name, _mappedAddress);
    }
}

