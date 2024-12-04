interface IHevm {
    function prank(address newSender) external;
    function startPrank(address sender) external;
    function stopPrank() external;
}

contract ExpectedCreator {
  event Sender(address);
  event AssertionFailed(string);
  IHevm constant vm = IHevm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);

  constructor(address s) public {
    if (s != msg.sender) {
      emit Sender(msg.sender);
      emit AssertionFailed("fail on construct");
    }
  }
}

contract SenderVerifierChild {
  event Sender(address);
  event AssertionFailed(string);
  IHevm constant vm = IHevm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);

  function verifyMsgSender(address s) public {
    if (s != msg.sender) {
      emit Sender(msg.sender);
      emit AssertionFailed("fail on inner call");
    }
  }
}

contract SenderVerifierParent {
  event Sender(address);
  event AssertionFailed(string);
  IHevm constant vm = IHevm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);

  SenderVerifierChild c;
  constructor() public {
    c = new SenderVerifierChild();
  }

  function verifyMsgSender(address s) public {
    if (s != msg.sender) {
      emit Sender(msg.sender);
      emit AssertionFailed("fail on first call");
    }
    c.verifyMsgSender(address(this));
    new ExpectedCreator(address(this));
  }
}

contract TestPrank {
  IHevm constant vm = IHevm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);

  SenderVerifierParent p;
  constructor() public {
    p = new SenderVerifierParent();
  }

  function withPrank() public {
    vm.prank(address(0x123));
    p.verifyMsgSender(address(0x123));
    p.verifyMsgSender(address(this));

    vm.prank(address(0x123));
    new ExpectedCreator(address(0x123));
    new ExpectedCreator(address(this));
  }

  function withStartPrank() public {
    vm.startPrank(address(0x123));
    p.verifyMsgSender(address(0x123));
    p.verifyMsgSender(address(0x123));
    new ExpectedCreator(address(0x123));
    new ExpectedCreator(address(0x123));
  }

  function withStartPrankStopPrank() public {
    vm.startPrank(address(0x123));
    p.verifyMsgSender(address(0x123));
    p.verifyMsgSender(address(0x123));
    new ExpectedCreator(address(0x123));
    new ExpectedCreator(address(0x123));
    vm.stopPrank();
    p.verifyMsgSender(address(this));
    new ExpectedCreator(address(this));
  }

  function withNothing() public {
    p.verifyMsgSender(address(this));
    new ExpectedCreator(address(this));
  }
}
