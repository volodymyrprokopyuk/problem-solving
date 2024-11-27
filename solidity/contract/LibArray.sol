// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

library LibArray {
  error ErrInvalidRange(uint start, uint end);

  function range(uint start, uint end)
    internal pure returns (uint[] memory) {
    require(start <= end, ErrInvalidRange(start, end));
    uint[] memory res = new uint[](end - start);
    for (uint i = start; i < end; i++) {
      res[i - start] = i;
    }
    return res;
  }

  function map(uint[] memory arr, function (uint) pure returns (uint) tr)
    internal pure returns (uint[] memory) {
    uint[] memory res = new uint[](arr.length);
    for (uint i = 0; i < arr.length; i++) {
      res[i] = tr(arr[i]);
    }
    return res;
  }

  function filter(
    uint[] memory arr, function (uint) pure returns (bool) pred,
    uint[] storage filtered
  ) internal {
    for (uint i = 0; i < arr.length; i++) {
      if (pred(arr[i])) {
        filtered.push(arr[i]);
      }
    }
  }

  function reduce(
    uint[] memory arr, uint init,
    function (uint, uint) pure returns (uint) comb)
    internal pure returns (uint) {
    uint res = init;
    for (uint i = 0; i < arr.length; i++) {
      res = comb(res, arr[i]);
    }
    return res;
  }
}
