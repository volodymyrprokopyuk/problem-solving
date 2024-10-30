// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {LibArray} from "contract/LibArray.sol";

contract LibArrayTest is Test {
  using LibArray for uint;

  uint[] public arr;

  function setUp() public {
    arr = LibArray.range(1, 4);
  }

  function mul10(uint value) internal pure returns (uint) {
    return value * 10;
  }

  function testMap() public view {
    uint[] memory got = LibArray.map(arr, mul10);
    uint[] memory exp = new uint[](got.length);
    exp[0] = 10; exp[1] = 20; exp[2] = 30;
    assertEq(got, exp);
  }

}
