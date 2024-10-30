// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

library LibArray {
  function range(uint start, uint end) internal pure
    returns (uint[] memory) {
    require(start <= end, "range: must start <= end");
    uint[] memory res = new uint[](end - start);
    for (uint i = start; i < end; i++) {
      res[i - start] = i;
    }
    return res;
  }

  function map(uint[] memory arr, function (uint) pure returns (uint) f)
    internal pure returns (uint[] memory) {
    uint[] memory res = new uint[](arr.length);
    for (uint i = 0; i < arr.length; i++) {
      res[i] = f(arr[i]);
    }
    return res;
  }
}
