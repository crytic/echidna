contract Foo {
    int256 private x;
    int256 private y;
    bool private state;
    constructor () public {
        x = 0;
        y = 0;
    }

    function Bar() public returns (int256) {
        if (x == 42) {
            state = true;
            return 1; 
        }
        return 0;
    }

    function SetY(int256 ny) public { 
	y = ny;
    }

    function IncX() public { 
	x++; 
    }

    function CopyY() public { 
	x = y; 
    }

    function echidna_assert() public returns (bool) {
        return !state;
    }
}
