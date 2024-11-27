// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

// solhint-disable no-console
import {Test, stdError, console} from "forge-std/Test.sol";
import {
  Greeting, Counter, ExhaustGas, Fallback, Delegatecall
} from "contract/Playground.sol";

contract GreetingTest is Test {
  Greeting internal greeting;

  function setUp() public {
    greeting = new Greeting();
  }

  function testGreetingGreet() public view {
    assertEq(greeting.greet(), "Hello");
  }
}

contract CounterTest is Test {
  Counter internal counter;

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
  ExhaustGas internal eg;

  function setUp() public {
    eg = new ExhaustGas();
  }

  function testDoNotExhaust() public {
    eg.exhaustGas{gas: 1e5}(1);
  }

  function testDoExhaust() public {
    try eg.exhaustGas{gas: 1e5}(type(uint).max) {
      fail();
    } catch {
      console.log("expected OutOfGas error");
    }
  }
}

contract EncodeTest is Test {
  Counter internal counter;

  function setUp() public {
    counter = new Counter();
  }

  function testEncodeSignature() public {
    address addr = address(counter);
    uint setValue = 1;
    bytes memory setData = abi.encodeWithSignature("set(uint256)", setValue);
    (bool setSucc, ) = addr.call(setData);
    assertTrue(setSucc);
    bytes memory getData = abi.encodeWithSignature("get()");
    (bool getSucc, bytes memory getRes) = addr.call(getData);
    assertTrue(getSucc);
    uint getValue = abi.decode(getRes, (uint));
    assertEq(getValue, setValue);
  }

  function testEncodeSelector() public {
    address addr = address(counter);
    uint setValue = 1;
    bytes4 setSelector = bytes4(keccak256("set(uint256)"));
    bytes memory setData = abi.encodeWithSelector(setSelector, setValue);
    // bytes memory setData = abi.encodeWithSelector(counter.set.selector, setValue);
    (bool setSucc, ) = addr.call(setData);
    assertTrue(setSucc);
    bytes4 getSelector = bytes4(keccak256("get()"));
    bytes memory getData = abi.encodeWithSelector(getSelector);
    // bytes memory getData = abi.encodeWithSelector(counter.get.selector);
    (bool getSucc, bytes memory getRes) = addr.call(getData);
    assertTrue(getSucc);
    uint getValue = abi.decode(getRes, (uint));
    assertEq(getValue, setValue);
  }

  function testEncodeCall() public {
    address addr = address(counter);
    uint setValue = 1;
    bytes memory setData = abi.encodeCall(counter.set, (setValue));
    (bool setSucc, ) = addr.call(setData);
    assertTrue(setSucc);
    bytes memory getData = abi.encodeCall(counter.get, ());
    (bool getSucc, bytes memory getRes) = addr.call(getData);
    assertTrue(getSucc);
    uint getValue = abi.decode(getRes, (uint));
    assertEq(getValue, setValue);
  }
}

contract FallbackTest is Test {
  Fallback internal fb;

  function setUp() public {
    fb = new Fallback();
  }

  function testFallback() public {
    bytes memory data = abi.encodeWithSignature("notExist(string)", "in");
    (bool success, bytes memory result) = address(fb).call(data);
    assertTrue(success);
    string memory res = abi.decode(result, (string));
    assertEq(res, "out");
    console.log("==> fallback: %s", res);

    // bytes memory data = abi.encodeWithSignature("notExist(uint256)", 1);
    // (bool success, bytes memory result) = address(fb).call(data);
    // assertTrue(success);
    // uint res = abi.decode(result, (uint));
    // assertEq(res, 2);
    // console.log("==> fallback: %s", res);
  }
}

contract DelegatecallTest is Test {
  Delegatecall internal dc1;
  Delegatecall internal dc2;

  function setUp() public {
    dc1 = new Delegatecall(1);
    dc2 = new Delegatecall(2);
  }

  function testDelegatecall() public {
    uint value1 = dc1.delegate(address(dc2), 1);
    assertEq(value1, 2);
    uint value2 = dc2.delegate(address(dc1), 1);
    assertEq(value2, 3);
  }
}

contract SignVerify is Test {
  function sign(bytes memory message, uint key)
    internal view returns (bytes memory) {
    bytes32 nonce = keccak256(abi.encode(block.timestamp));
    bytes32 hash = keccak256(abi.encode(message, nonce));
    (uint8 v, bytes32 r, bytes32 s) = vm.sign(key, hash);
    bytes memory sig = abi.encode(nonce, v, r, s);
    return sig;
  }

  function verify(bytes memory message, bytes memory sig, address addr)
    internal pure returns (bool) {
    (bytes32 nonce, uint8 v, bytes32 r, bytes32 s) =
      abi.decode(sig, (bytes32, uint8, bytes32, bytes32));
    bytes32 hash = keccak256(abi.encode(message, nonce));
    address signer = ecrecover(hash, v, r, s);
    return signer == addr;
  }

  function testSignVerify() public {
    (address addr, uint key) = makeAddrAndKey("account");
    bytes memory message = bytes("message");
    bytes memory sig = sign(message, key);
    bool valid = verify(message, sig, addr);
    assertTrue(valid);
  }
}
