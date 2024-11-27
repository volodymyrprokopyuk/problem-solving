// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {LibArray} from "contract/LibArray.sol";

contract LibArrayTest is Test {
  uint[] internal arr;
  uint[] internal filtered; // Only storage arrays grow automatically

  using LibArray for uint[]; // Attach all library functions to uint[]
  // using {LibArray.map} for uint[]; // Attach selected library functions to uint[]

  function setUp() public {
    arr = LibArray.range(1, 4); // Library reference
  }

  function mul10(uint value) internal pure returns (uint) {
    return value * 10;
  }

  function testMap() public view {
    // uint[] memory got = LibArray.map(arr, mul10); // Library reference
    uint[] memory got = arr.map(mul10); // Attached library function
    uint[] memory exp = new uint[](got.length);
    exp[0] = 10; exp[1] = 20; exp[2] = 30;
    assertEq(got, exp);
  }

  function even(uint value) internal pure returns (bool) {
    return value % 2 == 0;
  }

  function testFilter() public {
    arr.filter(even, filtered);
    uint[] memory exp = new uint[](1);
    exp[0] = 2;
    assertEq(filtered, exp);
  }

  function add(uint a, uint b) internal pure returns (uint) {
    return a + b;
  }

  function testReduce() public view {
    uint got = arr.reduce(0, add);
    uint exp = 1 + 2 + 3;
    assertEq(got, exp);
  }
}
