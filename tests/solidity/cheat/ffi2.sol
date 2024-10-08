pragma experimental ABIEncoderV2;

interface Hevm {
  function setEnv(string calldata, string calldata) external;
  function ffi(string[] calldata) external returns (bytes memory);
}

contract TestFFI {
  address constant HEVM_ADDRESS = 0x7109709ECfa91a80626fF3989D68f67F5b1DD12D;

  bytes32 hehe;

  function foo(int x) external {
    // ABI encoded "gm", as a string
    Hevm(HEVM_ADDRESS).setEnv("ECHIDNA_FOO_BAR", "0x00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000002676d000000000000000000000000000000000000000000000000000000000000");

    string[] memory inputs = new string[](3);
    inputs[0] = "sh";
    inputs[1] = "-c";
    inputs[2] = "printf '%s' \"$ECHIDNA_FOO_BAR\"";

    bytes memory res = Hevm(HEVM_ADDRESS).ffi(inputs);

    (string memory output) = abi.decode(res, (string));
    hehe = keccak256(bytes(output));
  }

  function echidna_ffi() public returns (bool){
    return hehe != keccak256("gm");
  }
}
