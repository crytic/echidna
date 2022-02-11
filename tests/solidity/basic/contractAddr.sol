contract Foo {
    function f() public {}

    function echidna_address() public returns (bool) {
        return address(this) == address(0x12342);
    }
}
