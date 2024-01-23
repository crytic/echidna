contract VulnerableContract {
    mapping (uint256 => uint256) a;
    function func_one(uint256 x) public payable {
        a[12323] = ((x >> 5) / 7);
        if (a[12323] == 2) {
            assert(false); // BUG
        }
    }

    function func_two(uint256 x) public payable {
        if ((x >> 5) / 7 == 2) {
            assert(false); // BUG
        }
    }
}
