// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {console} from "forge-std/console.sol";

contract Greeting {
  string public greet = "Hello"; // In-place initialization
}

contract Counter {
  uint private count;

  function set(uint value) public {
    count = value;
  }

  function get() public view returns (uint) {
    return count;
  }

  function dec(uint delta) public {
    count -= delta;
  }
}

contract ExhaustGas {
  uint i;
  function exhaustGas(uint limit) public {
    while (true) {
      i += 1;
      if (i >= limit) {
        break;
      }
    }
  }
}

contract Fallback {
  fallback(bytes calldata data) external returns (bytes memory) {
    // string memory arg = abi.decode(data[4:], (string));
    // console.log("fallback: %s", arg);
    // return abi.encode("out");

    uint arg = abi.decode(data[4:], (uint));
    console.log("fallback: %s", arg);
    return abi.encode(arg + 1);
  }
}

contract Delegatecall {
  uint value;

  constructor (uint val) {
    value = val;
  }

  function inc(uint delta) public returns (uint) {
    value += delta;
    return value;
  }

  function delegate(address delegee, uint delta) public returns (uint) {
    bytes memory data = abi.encodeWithSignature("inc(uint256)", delta);
    (bool success, bytes memory result) = delegee.delegatecall(data);
    require(success, "inc delegate failure");
    return abi.decode(result, (uint));
  }
}
