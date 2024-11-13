// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {console} from "forge-std/console.sol";

contract Greeting {
  string public greet = "Hello";
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

// contract Fallback {
//   fallback(bytes memory data) external returns (bytes memory) {
//     console.log("fallback data: %s", abi.decodePacked(data));
//     return abi.encodePacked("ok");
//   }
// }
