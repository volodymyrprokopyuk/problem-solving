// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test, stdError} from "forge-std/Test.sol";
import {Greeting, Counter} from "contract/Playground.sol";

contract GreetingTest is Test {
  Greeting greeting;

  function setUp() public {
    greeting = new Greeting();
  }

  function testGreetingGreet() public view {
    assertEq(greeting.greet(), "Hello");
  }
}

contract CounterTest is Test {
  Counter counter;

  function setUp() public {
    counter = new Counter();
  }

  function testCounterSetGetDec() public {
    counter.set(1);
    assertEq(counter.get(), 1);
    counter.dec(1);
    assertEq(counter.get(), 0);
    vm.expectRevert(stdError.arithmeticError);
    counter.dec(1); // arithmetic underflow
  }
}
