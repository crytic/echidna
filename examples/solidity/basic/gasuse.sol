contract C {

  bool open1 = false;
  bool open2 = false;
  
  uint256[500] a;

  uint256[] b;
 
 function f_open1() public {
   open1 = true;
 }

 function f_open2() public {
   open2 = true;
 }

 function f_close1() public {
   open1 = false;
 }

 function f_close2() public {
   open2 = false;
 }

 function push_b(uint256 n) public {
   b.push(n);
 }

 function g(uint256 n) public {
   if (open1 && open2) {
     if (n < 500) {
       for (uint256 i = 0; i < n; i++) {
	 uint256 sum = 0;
	 for (uint256 j = 0; j < b.length; j++) {
	   sum += b[j];
	 }
	 a[i] = sum;
       }
     }
   }
 }

 function echidna_true() public returns (bool) {
   return true;
 }
 
}
