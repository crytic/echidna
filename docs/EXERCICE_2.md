# Exercice 2

This exercice requires to finish the [exercice 1](EXERCICE_1.md).

**Table of contents:**
- [Targeted contract](#targeted-contract)
- [Exercice](#exercice)
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

Consider `paused()` to be called at deployment, and the ownership removed.

Add a property to check that the contract cannot be unpaused.

The skeleton for this exercise is (*[exercises/exercise2/exercise2.sol](https://github.com/trailofbits/publications/blob/master/workshops/Automated%20Smart%20Contracts%20Audit%20-%20TruffleCon%202018/echidna/exercises/exercise2/exercise2.sol)*):
   
```Solidity
   import "token.sol";
   contract TestToken is Token {
      address echidna_caller = 0x00a329c0648769a73afac7f9381e08fb43dbea70;
      constructor(){
         paused(); // pause the contract
         owner = 0x0; // lose ownership
       }
         // add the property
     }
```
       
Once Echidna found the bug, fix the issue, and re-try your property with Echidna.
   
## Solution

   
   
