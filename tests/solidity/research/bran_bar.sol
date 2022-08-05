contract C { 

function bar(uint256 w,uint256 x, uint256 y, uint256 z, uint256 a) public returns (uint256) 
{
  uint256 ret = 0;
  if (x % 2 == 0) {  //if(x % 1000 != 42) {
    ret = 256;
    if (y % 2 == 0) {
      ret = 257;
    }
    w = w % ret;
    while (w != 0) { 
      w--;
    } 
    assert(w == 0);   //drop this line
    z = z % ret;
    while(ret != z) {
      z++;
    }
    assert(ret == z); //assert(x != 42 - w*z);
  } else{ 
    ret = 3*a*a + 7*a + 101;
    assert(ret != 5687);
  }
  
  return ret;
}

}
