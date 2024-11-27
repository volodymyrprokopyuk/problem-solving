// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test, stdError} from "forge-std/Test.sol";
import {ErrorHandling} from "contract/ErrorHandling.sol";

contract ErrorHandlingTest is Test {
  ErrorHandling internal eh;

  function setUp() public {
    eh = new ErrorHandling();
  }

  function testSuccess() public {
    try eh.produceError(ErrorHandling.ErrorType.Success)
      returns (string memory message) {
      assertEq(message, "success");
    } catch {
      fail();
    }
  }

  function testExplicitAssert() public {
    // * Manual assertion
    try eh.produceError(ErrorHandling.ErrorType.ExplicitAssert)
      returns (string memory){
      fail();
    } catch Panic(uint code) {
      assertEq(code, 1);
    }
    // * Forge assertion
    vm.expectRevert(stdError.assertionError);
    eh.produceError(ErrorHandling.ErrorType.ExplicitAssert);
  }

  function testImplicitPanic() public {
    // * Manual assertion
    try eh.produceError(ErrorHandling.ErrorType.ImplicitPanic)
      returns (string memory) {
      fail();
    } catch Panic(uint code) {
      assertEq(code, 17);
    }
    // * Forge assertion
    vm.expectRevert(stdError.arithmeticError);
    eh.produceError(ErrorHandling.ErrorType.ImplicitPanic);
  }

  function testRevertCustomError() public {
    bytes memory expErr = abi.encodeWithSignature("ErrOh(string)", "revert error");
    // * Manual assertion
    try eh.produceError(ErrorHandling.ErrorType.Revert)
      returns (string memory) {
      fail();
    } catch (bytes memory err) { // custom errors must be handled as bytes
      // Signature match
      assertEq(err, expErr);
      // Selector match
      assertEq(bytes4(err), ErrorHandling.ErrOh.selector);
    }
    // * Forge assertion
    // Signature match
    vm.expectRevert(expErr);
    eh.produceError(ErrorHandling.ErrorType.Revert);
    // Selector match
    vm.expectPartialRevert(ErrorHandling.ErrOh.selector);
    eh.produceError(ErrorHandling.ErrorType.Revert);
  }

  function testRequireError() public {
    // * Manual assertion
    try eh.produceError(ErrorHandling.ErrorType.Require)
      returns (string memory) {
      fail();
    } catch Error(string memory message) {
      assertEq(message, "require error");
    }
    // * Forge assertion
    vm.expectRevert("require error");
    eh.produceError(ErrorHandling.ErrorType.Require);
  }
}
