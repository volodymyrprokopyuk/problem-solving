// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract SimpleStorage {
  uint256 value;

  function set(uint256 val) public {
    value = val;
  }

  function get() public view returns (uint256) {
    return value;
  }
}
