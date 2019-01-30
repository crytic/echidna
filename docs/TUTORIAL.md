# Echidna Tutorial

The aim of this document is to show how to use Echidna to automatically test smart contracts.

The first part introduces how to write a property for Echidna. 
The second part is a set of exercices to solve. 
The last part contains advanced usages.

**Table of contents:**
- [Requirements](#requirements)
- [How to test a property](TESTING_A_PROPERTY.md)
- [Exercise 1](EXERCISE_1.md)
- [Exercise 2](EXERCISE_2.md)
- [Exercise 3](EXERCISE_3.md)
- [Advanced: How to use Echidna with multiple contracts](#TODO)
- [Advanced: How to write efficient properties](#TODO)

Join the team on Slack at: https://empireslacking.herokuapp.com/ #ethereum

## Requirements

All the material of this tutorial is included in our [eth-security-toolbox](https://github.com/trailofbits/eth-security-toolbox). Run:
```         
$ docker pull trailofbits/eth-security-toolbox
$ docker run -it trailofbits/eth-security-toolbox
```

The examples and the exercices are inspired from our [TruffleCon workshop](https://github.com/trailofbits/publications/tree/master/workshops/Automated%20Smart%20Contracts%20Audit%20-%20TruffleCon%202018).
Once in the docker, run:
```
cd "/home/ethsec/workshops/Automated Smart Contracts Audit - TruffleCon 2018/echidna"
```

  
  
