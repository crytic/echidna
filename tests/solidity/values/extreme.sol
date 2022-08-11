contract Test {
  int8 private constant MIN8 = -128;  
  int16 private constant MIN16 = -32768;
  int32 private constant MIN32 = -2147483648;
  int64 private constant MIN64 = -9223372036854775808;
  int128 private constant MININT128 = -170141183460469231731687303715884105728;

  function testMinInt8(int8 xzgx) public {
    if (xzgx == MIN8)
      assert(false);
  }  

  function testMinInt16(int16 xzgx) public {
    if (xzgx == MIN16)
      assert(false);
  } 

  function testMinInt32(int32 xzgx) public {
    if (xzgx == MIN32)
      assert(false);
  } 

  function testMinInt64(int64 xzgx) public {
    if (xzgx == MIN64)
      assert(false);
  }

  function testMinInt128(int128 xzgx) public {
    if (xzgx == MININT128)
      assert(false); 
  }
}
