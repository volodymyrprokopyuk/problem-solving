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

contract EncodeTest is Test {
  Counter counter;

  function setUp() public {
    counter = new Counter();
  }

  function testEncodeSignature() public {
    address ctr = address(counter);
    uint setValue = 1;
    bytes memory setData = abi.encodeWithSignature("set(uint256)", setValue);
    (bool setSucc, ) = ctr.call(setData);
    require(setSucc, "set failure");
    bytes memory getData = abi.encodeWithSignature("get()");
    (bool getSucc, bytes memory getRes) = ctr.call(getData);
    require(getSucc, "get failure");
    (uint getValue) = abi.decode(getRes, (uint));
    assertEq(getValue, setValue);
  }

  function testEncodeSelector() public {
    address ctr = address(counter);
    uint setValue = 1;
    bytes4 setSelector = bytes4(keccak256("set(uint256)"));
    bytes memory setData = abi.encodeWithSelector(setSelector, setValue);
    // bytes memory setData = abi.encodeWithSelector(counter.set.selector, setValue);
    (bool setSucc, ) = ctr.call(setData);
    require(setSucc, "set failure");
    bytes4 getSelector = bytes4(keccak256("get()"));
    bytes memory getData = abi.encodeWithSelector(getSelector);
    // bytes memory getData = abi.encodeWithSelector(counter.get.selector);
    (bool getSucc, bytes memory getRes) = ctr.call(getData);
    require(getSucc, "get failure");
    (uint getValue) = abi.decode(getRes, (uint));
    assertEq(getValue, setValue);
  }

  function testEncodeCall() public {
    address ctr = address(counter);
    uint setValue = 1;
    bytes memory setData = abi.encodeCall(counter.set, (setValue));
    (bool setSucc, ) = ctr.call(setData);
    require(setSucc, "set failure");
    bytes memory getData = abi.encodeCall(counter.get, ());
    (bool getSucc, bytes memory getRes) = ctr.call(getData);
    require(getSucc, "get failure");
    (uint getValue) = abi.decode(getRes, (uint));
    assertEq(getValue, setValue);
  }
}

// contract FallbackTest is Test {
//   Fallback fb;

//   function setUp() public {
//     fb = new Fallback();
//   }

//   function testFallback() public {
//     (bool success, bytes memory data) = fb.call(abi.encodePacked("ok"));
//   }
// }
