contract Test {
    bytes16 key = "\xf8";
    function testNonUTF8(string memory input) public {
       bytes memory x = bytes(input);
       if (x.length > 0)
         assert(key[0] != x[0]);
    }
}
