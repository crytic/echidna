# Exercise 3 
This exercice requires to finish the [exercise 1](EXERCISE_1.md) and the [exercise 2](EXERCISE_2.md).

**Table of contents:**
- [Targeted contract](#targeted-contract)
- [Exercise](#exercice)
- [Solution](#solution)

Join the team on Slack at: https://empireslacking.herokuapp.com/ #ethereum

All the paths given in this page are relative to `/home/ethsec/workshops/Automated Smart Contracts Audit - TruffleCon 2018/echidna` in [eth-security-toolbox](https://github.com/trailofbits/eth-security-toolbox`).
 
## Targeted contract
  
We will test the following contract *[exercises/token.sol](https://github.com/trailofbits/publications/blob/master/workshops/Automated%20Smart%20Contracts%20Audit%20-%20TruffleCon%202018/echidna/exercises/token.sol)*:
       
```Solidity
 contract Ownership{
    address owner = msg.sender;
    function Owner(){
        owner = msg.sender;
     }
     modifier isOwner(){
         require(owner == msg.sender);
         _;
      }
   }
        
  contract Pausable is Ownership{
     bool is_paused;
     modifier ifNotPaused(){
                require(!is_paused);
               _;
      }
       
      function paused() isOwner public{
          is_paused = true;
      }
          
      function resume() isOwner public{
          is_paused = false;
      }
   }
      
   contract Token is Pausable{
      mapping(address => uint) public balances;
      function transfer(address to, uint value) ifNotPaused public{
           balances[msg.sender] -= value;
           balances[to] += value;
       }
    }
    
```
     
## Exercise

Consider the following extension of the token (*[exercises/bonus/bonus.sol](https://github.com/trailofbits/publications/blob/master/workshops/Automated%20Smart%20Contracts%20Audit%20-%20TruffleCon%202018/echidna/exercises/bonus/bonus.sol)*):
   
```Solidity
   import "token.sol"; 
   contract MintableToken is Token{
      int totalMinted;
      int totalMintable;
          
      function MintableToken(int _totalMintable){
         totalMintable = _totalMintable;
      }
          
      function mint(uint value) isOwner(){
          require(int(value) + totalMinted < totalMintable);
          totalMinted += int(value);
          balances[msg.sender] += value;
       }
    }
    
```
      
Use the [version of token.sol](https://github.com/trailofbits/publications/blob/master/workshops/Automated%20Smart%20Contracts%20Audit%20-%20TruffleCon%202018/echidna/exercises/bonus/token.sol#L1) containing the fixes of the previous exercices.
   
Create a scenario, where `echidna_caller (0x00a329c0648769a73afac7f9381e08fb43dbea70)`          becomes the owner of the contract at construction, and `totalMintable` is set to 10,000.        Recall that Echidna needs a constructor without argument.

Add a property to check if `echidna_caller` cannot mint more than 10,000 tokens.

Once Echidna found the bug, fix the issue, and re-try your property with Echidna.
   
## Solution

This solution can be found in ```/exercises/bonus/bonus_solution.sol``` or click [here](https://github.com/trailofbits/publications/blob/master/workshops/Automated%20Smart%20Contracts%20Audit%20-%20TruffleCon%202018/echidna/exercises/bonus/bonus_solution.sol)

```Solidity
import "bonus.sol";


contract TestToken is MintableToken{

    address echidna_caller = 0x00a329c0648769a73afac7f9381e08fb43dbea70;
    function TestToken() MintableToken(10000){
        owner = echidna_caller;
    }

    // add the property
    function echidna_test_balance() view public returns(bool){
        return balances[msg.sender] <= 10000;
    }   



}

```
