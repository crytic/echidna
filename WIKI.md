## Getting started

Use our prebuilt Docker container to quickly install and run the toolkit:
         
         $ docker pull trailofbits/eth-security-toolbox
         $ docker run -it trailofbits/eth-security-toolbox

Alternatively, build the image from scratch:

        $ git clone https://github.com/trailofbits/eth-security-toolbox.git
        $ cd eth-security-toolbox
        $ docker build -t eth-security-toolbox .

All the material of this workshop is available in https://github.com/trailofbits/publications/.

The aim of this document is to show how to use Echidna to automatically test smart contracts.
The goal of the workshop is to solve the exercices proposed in Section 2. Section 1 introduces
how to write a property for Echidna.

Join the team on Slack at: https://empireslacking.herokuapp.com/ #ethereum

## Testing a property

 We will see how to test a smart contract with Echidna. The target is the following smart contract

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
    -  Anyone can have at maximum 1000 tokens
    - The token cannot be transferred (it is not an erc20 token)


## Writing a property
 
 Echidna properties are Solidity functions. A property must:
     - Have no argument
     - Not change the state (i.e. it is a `view` function)
     - Return true if it is successful
     - Have its name starting with `echidna`
 
 Echidna will automatically generate transactions to make the property returning false, or throw
 an error.
 
 The following property checks that the caller has no more than 1000 tokens:
 
        ```function echidna_balance_under_1000() public view returns(bool){
                return balances[msg.sender] <= 1000;
            }```
            
 Use inheritance to separate your contract from your properties:
    
       ```contract TestToken is Token{
            function echidna_balance_under_1000() public view returns(bool){
                return balances[msg.sender] <= 1000;
            }
        }```
  
## Initiate your contract
 
 Echidna needs a constructor without argument.
 If your contract needs a specific initialization, you need to do it in the constructor.
 
 There are two specific addresses in Echidna:
      - `0x00a329c0648769a73afac7f9381e08fb43dbea72` which calls the constructor.
      - `0x00a329c0648769a73afac7f9381e08fb43dbea70` which calls the other functions.
      
## Running Echidna
 
 Echidna is launched with:
 
        `$ echidna-test contract.sol`
        
 If contract.sol contains multiple contracts, you can specify the target:
 
        `$ echidna-test contract.sol MyContract`
 
## Summary: Testing a property
 
 The following summarizes the run of echidna on our example:
 
         ```contract TestToken is Token{
               constructor() public {}
               
               function echidna_balance_under_1000() public view returns(bool){
                    return balances[msg.sender] <= 1000;
               }
            }```
            
           
         ```$ echidna-test testtoken.sol TestToken
            ...
               ✗ "echidna_balance_under_1000" failed after 29 tests and 1
             shrink.
             
                 │ Call sequence: airdrop();
                 │ backdoor();
              ✗ 1 failed.```
    
  Echidna found that the property is violated if `backdoor` is called.
  
  
## Exercises
  
  The two following exercises are meant to be solved using Echidna.
  
## Targeted contract
  
  We will test the following contract:
       
      ```contract Ownership{
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
     }```
     
     
## Exercise 1

   Add a property to check that `echidna_caller` cannot have more than an initial balance of
   10000.
   
   The skeleton for this exercise is:
   
     ```import "token.sol";
        
        contract TestToken is Token {
            address echidna_caller = 0x00a329c0648769a73afac7f9381e08fb43dbea70;

            constructor() public{
                 balances[echidna_caller] = 10000;
               }
           // add the property
       }```
       
   Once Echidna found the bug, fix the issue, and re-try your property with Echidna.
   
## Exercise 2

   Now paused is called at deployment, and the ownership is removed.
   Add a property to check that the contract cannot be unpaused.
   
   The skeleton for this exercise is:
   
     ```import "token.sol";
        
        contract TestToken is Token {
            address echidna_caller = 0x00a329c0648769a73afac7f9381e08fb43dbea70;
            constructor(){
                paused(); // pause the contract
                owner = 0x0; // lose ownership
            }
            // add the property
         }```
       
   Once Echidna found the bug, fix the issue, and re-try your property with Echidna.
   
## Exercise 3 (Bonus)

   Consider the following extension of the token:
   
   ```import "token.sol";
      
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
      }```
      
   Use the [version of token.sol] ( https://github.com/trailofbits/publications/blob/master/workshops/Automated%20Smart%20Contracts%20Audit%20-%20TruffleCon%202018/echidna/exercises/bonus/token.sol#L1) containing the fixes of the previous exercices.
   
   Create a scenario, where `echidna_caller (0x00a329c0648769a73afac7f9381e08fb43dbea70)`          becomes the owner of the contract at construction, and `totalMintable` is set to 10,000.        Recall that Echidna needs a constructor without argument.
   
   Add a property to check if `echidna_caller` cannot mint more than 10,000 tokens.
   
   Once Echidna found the bug, fix the issue, and re-try your property with Echidna.
   
## Using Asserts
  An assertion is a Boolean expression at a specific point in a program which will be true unless   there is a bug in the program. 
  Using asserts with echidna can be really helpful in zeroing in on the bugs in your code. Solidity   language supports the assert() function which takes in a condition and determines the boolean       value to determine the outcome of your script. 
  
   
