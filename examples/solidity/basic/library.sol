library Test{
    struct Storage{
        bool flag;
    }

    function set(Storage storage st) public{
        st.flag = true;
    }

}

contract Contract{
    using Test for Test.Storage;
    Test.Storage st;

    function set() public{
        st.set();
    }

    function echidna_library_call() external view returns (bool) {
        return (!st.flag);
    }
}
