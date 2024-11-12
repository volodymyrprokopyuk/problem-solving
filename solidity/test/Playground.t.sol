// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test, stdError, console} from "forge-std/Test.sol";
import {
  Greeting, Counter, ExhaustGas
} from "contract/Playground.sol";

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

contract ExhaustGasTest is Test {
  ExhaustGas eg;

  function setUp() public {
    eg = new ExhaustGas();
  }

  function testExhaustGasDoNotExhaust() public {
    eg.exhaustGas{gas: 1e5}(1);
  }

  function testExhaustGasDoExhaust() public {
    try eg.exhaustGas{gas: 1e5}(type(uint).max) {
      fail();
    } catch {
      console.log("expected OutOfGas error");
    }
  }
}
