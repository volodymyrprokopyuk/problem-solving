// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {IERC165, IFuncs, ERC165Contract} from "contract/ERC165Interface.sol";

contract ERC165ContractTest is Test {
  ERC165Contract contr;

  function setUp() public {
    contr = new ERC165Contract();
  }

  function testDetectInterface() public {
    // Detect ERC165 interface
    bytes memory data = abi.encodeWithSelector(
      IERC165.supportsInterface.selector, type(IERC165).interfaceId
    );
    (bool success, bytes memory result) = address(contr).call(data);
    assertTrue(success);
    (bool isIERC165) = abi.decode(result, (bool));
    assertTrue(isIERC165);
    // Detect IFuncs interface
    assertTrue(contr.supportsInterface(type(IFuncs).interfaceId));
    IFuncs funcs = contr;
    assertEq(funcs.funcA(), 1);
    assertEq(funcs.funcB(), 2);
  }
}
