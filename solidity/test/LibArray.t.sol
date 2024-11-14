// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {LibArray} from "contract/LibArray.sol";

contract LibArrayTest is Test {
  uint[] public arr;
  uint[] public filtered;

  function setUp() public {
    arr = LibArray.range(1, 4);
  }

  function mul10(uint value) internal pure returns (uint) {
    return value * 10;
  }

  function testLibraryMap() public view {
    uint[] memory got = LibArray.map(arr, mul10);
    uint[] memory exp = new uint[](got.length);
    exp[0] = 10; exp[1] = 20; exp[2] = 30;
    assertEq(got, exp);
  }

  using LibArray for uint[];
  // using {LibArray.map} for uint[];

  function testUsingMap() public view {
    uint[] memory got = arr.map(mul10);
    uint[] memory exp = new uint[](got.length);
    exp[0] = 10; exp[1] = 20; exp[2] = 30;
    assertEq(got, exp);
  }

  function even(uint value) internal pure returns (bool) {
    return value % 2 == 0;
  }

  function testFilter() public {
    arr.filter(filtered, even);
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
