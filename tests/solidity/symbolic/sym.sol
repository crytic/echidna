contract VulnerableContract {
   function func_one(int256 x) public pure {
     if (x / 4 == -20) {
       assert(false); // BUG
     }
   }

   function func_two(int128 x) public payable {
     if ((msg.value >> 30) / 7 == 2) {
       assert(false); // BUG
     }
   }
}
