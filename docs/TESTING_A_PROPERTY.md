# Testing a Property with Echidna

**Table of contents:**

- [Introduction](#introduction)
- [How to write a property](#how-to-write-a-property)
- [How to initiate a contract](#how-to-initiate-a-contract)
- [How to run Echidna](#how-to-run-Echidna)

All the paths given in this tutorial are relative to `/home/ethsec/workshops/Automated Smart Contracts Audit - TruffleCon 2018/echidna` in [eth-security-toolbox](https://github.com/trailofbits/eth-security-toolbox`).

## Introduction

We will see how to test a smart contract with Echidna. The target is the following smart contract (*[example/token.sol](https://github.com/trailofbits/publications/blob/master/workshops/Automated%20Smart%20Contracts%20Audit%20-%20TruffleCon%202018/echidna/example/token.sol)*):

```Solidity
   contract Token{
      mapping(address => uint) public balances;
      function airdrop() public{
          balances[msg.sender] = 1000;
     }
     function consume() public{
          require(balances[msg.sender]>0);
          balances[msg.sender] -= 1;
     }
     function backdoor() public{
          balances[msg.sender] += 1;
     }
   }   
  
```

We will make the assumption that this token must have the following properties:
- Anyone can have at maximum 1000 tokens
- The token cannot be transferred (it is not an ERC20 token)

## How to write a property
Echidna properties are Solidity functions. A property must:
 - Have no argument
 - Not change the state (i.e. it is a `view` function)
 - Return true if it is successful
 - Have its name starting with `echidna`
 
Echidna will automatically generate transactions to make the property returning false, or throw
an error.

The following property checks that the caller has no more than 1000 tokens:
```Solidity
    function echidna_balance_under_1000() public view returns(bool){
         return balances[msg.sender] <= 1000;
    }
```
Use inheritance to separate your contract from your properties:
```Solidity
    contract TestToken is Token{
         function echidna_balance_under_1000() public view returns(bool){
               return balances[msg.sender] <= 1000;
          }
     }
```

*[example/testtoken.sol](https://github.com/trailofbits/publications/blob/master/workshops/Automated%20Smart%20Contracts%20Audit%20-%20TruffleCon%202018/echidna/example/testtoken.sol)* implements the property and inherits from the token.
 
## How to initiate a contract
 
Echidna needs a constructor without argument.
If your contract needs a specific initialization, you need to do it in the constructor.

There are two specific addresses in Echidna:
* `0x00a329c0648769a73afac7f9381e08fb43dbea72` which calls the constructor.
* `0x00a329c0648769a73afac7f9381e08fb43dbea70` which calls the other functions.

We do not need any particular initialization in our current example, as a result our constructor is empty.
      
## How to run Echidna
 
Echidna is launched with:

```bash
$ echidna-test contract.sol
```
       
If contract.sol contains multiple contracts, you can specify the target:

```bash
$ echidna-test contract.sol MyContract
```
 
## Summary: Testing a property
 
The following summarizes the run of echidna on our example:
```Solidity
     contract TestToken is Token{
         constructor() public {}
             function echidna_balance_under_1000() public view returns(bool){
                return balances[msg.sender] <= 1000;
             }
       }
```
          
```bash
$ echidna-test testtoken.sol TestToken
    ...
      ✗ "echidna_balance_under_1000" failed after 29 tests and 1 shrink.
            
         │ Call sequence: airdrop();
         │ backdoor();
           ✗ 1 failed.
```
   
 Echidna found that the property is violated if `backdoor` is called.
  

