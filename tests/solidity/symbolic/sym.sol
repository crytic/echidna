contract VulnerableContract {
    mapping (uint256 => uint256) a;
    bool y;
    bool z;
    function func_one(uint256 x) public payable {
        a[12323] = ((x >> 5) / 7777);
        if (a[12323] == 2222) {
            y = true;
        }
    }

    function func_two(uint256 x) public payable {
        if ((x >> 5) / 7777 == 2222) {
            z = true;
        }
    }

    function echidna_sym() public returns (bool) {
        return !(y && z);
    }
}
