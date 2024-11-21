// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

interface IERC165 {
  function supportsInterface(bytes4 ifcID) external view returns (bool);
}

interface IFuncs {
  function funcA() external view returns (uint);
  function funcB() external view returns (uint);
}

contract ERC165Contract is IERC165, IFuncs {
  uint a = 1;
  uint b = 2;
  mapping(bytes4 => bool) public supportsInterface;

  constructor() {
    supportsInterface[type(IERC165).interfaceId] = true;
    supportsInterface[type(IFuncs).interfaceId] = true;
  }

  function funcA() external view returns (uint) {
    return a;
  }

  function funcB() external view returns (uint) {
    return b;
  }
}
