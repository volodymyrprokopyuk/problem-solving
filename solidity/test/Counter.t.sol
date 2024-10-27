// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import {Test, console} from "forge-std/Test.sol";
import {Counter} from "contract/Counter.sol";

contract CounterTest is Test {
  Counter public counter;

  function setUp() public {
    counter = new Counter();
    counter.setNumber(0);
  }

  function testIncrement() public {
    counter.increment();
    assertEq(counter.number(), 1);
  }

  function testFailNonOwnerIncrement() public {
    vm.prank(address(0));
    counter.increment();
  }

  function testRevertNonOwnerIncrement() public {
    vm.expectRevert(Counter.ErrOnlyOwner.selector);
    vm.prank(address(0));
    counter.increment();
  }

  function testEmitIncrement() public {
    // indexed topic1, topic2, topic3, non-indexed data
    vm.expectEmit(true, false, false, true);
    emit Counter.EvIncrement(1); // expected event
    counter.increment(); // actual event
  }

  function testFailDoRevert() public view {
    counter.doRevert();
  }

  function testFuzzSetNumber(uint256 x) public {
    counter.setNumber(x);
    assertEq(counter.number(), x);
  }
}

abstract contract SharedSetup {
  uint256 public constant number = 1234;
}

contract MultiTxTest is Test, SharedSetup {
  uint256 a;
  uint256 b;

  function beforeTestSetup(bytes4 testSelector) public pure
    returns (bytes[] memory) {
    if (testSelector == this.testMultiTx.selector) {
      bytes[] memory testCalldata = new bytes[](2);
      testCalldata[0] = abi.encodePacked(this.testStateless.selector);
      testCalldata[1] = abi.encodeWithSignature("setUpMultiTx(uint256)", number);
      return testCalldata;
    }
    return new bytes[](0);
  }

  function testStateless() public {
    console.log("=> testA");
    require(a == 0);
    a = 1;
  }

  function setUpMultiTx(uint256 value) public {
    console.log("=> setUpMultiTx");
    b = value;
  }

  function testMultiTx() public view {
    assertEq(a, 1);
    assertEq(b, number);
  }
}
