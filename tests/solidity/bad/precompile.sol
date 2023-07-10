// tests unwind support in precompiles
contract Unwind {
    constructor() public {
        bytes32 x = hex"01";
        bytes32[4] memory input;
        bytes32[2] memory result;
        input[0] = x;
        input[1] = x;
        input[2] = x;
        input[3] = x;
        assembly {
            let success := call(gas(), 0x06, 0, input, 0x80, result, 0x40)
            switch success
            case 0 {
                revert(0, 0)
            }
        }
    }

    function f() pure public returns (int) {
        return 0;
    }

    function echidna_f() pure public returns (bool) {
        return false;
    }
}