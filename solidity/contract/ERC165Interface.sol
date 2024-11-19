// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

interface IERC165 {
  function supportsInterface(bytes4 ifcID) external view returns (bool);
}

function IERC165ID() pure returns (bytes4) {
  return bytes4(IERC165.supportsInterface.selector);
}

interface IFuncs {
  function funcA() external view returns (uint);
  function funcB() external view returns (uint);
}

function IFuncsID() pure returns (bytes4) {
  return bytes4(IFuncs.funcA.selector) ^ bytes4(IFuncs.funcB.selector);
}

contract ERC165Contract is IERC165, IFuncs {
  uint a = 1;
  uint b = 2;
  mapping(bytes4 => bool) public supportsInterface;

  constructor() {
    supportsInterface[IERC165ID()] = true;
    supportsInterface[IFuncsID()] = true;
  }

  function funcA() external view returns (uint) {
    return a;
  }

  function funcB() external view returns (uint) {
    return b;
  }
}
