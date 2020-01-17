contract A{
    bool public bug = true;
    function trigger_bug() public{
        bug = false;
    }
}
contract B{
    A public a;
    constructor() public{
        a = new A();
    }
    function echidna_test() public returns(bool){
        return a.bug();
    }
}
