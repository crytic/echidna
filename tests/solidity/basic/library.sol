library Test {
    struct Storage{
        bool flag;
    }

    function set(Storage storage st) public{
        st.flag = true;
    }

 }

contract Contract {
    using Test for Test.Storage;
    Test.Storage st;

    function set() public{
        st.set();
    }

    function echidna_library_call() external view returns (bool) {
        return (!st.flag);
    }

    function echidna_valid_timestamp() external view returns (bool) {
        require(block.timestamp >= 1524785992 && block.number >= 4370000);
        return (block.timestamp <= 1524785992 + 100 weeks && block.number < 4370000 + 10000000);
    }
}
