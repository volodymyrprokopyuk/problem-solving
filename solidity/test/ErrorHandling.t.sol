// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test, stdError} from "forge-std/Test.sol";
import {ErrorHandling} from "contract/ErrorHandling.sol";

contract ErrorHandlingTest is Test {
  ErrorHandling eh;

  error ErrExpectedErrorGotNone();

  function setUp() public {
    eh = new ErrorHandling();
  }

  function testErrorHandlingSuccess() public view {
    string memory message = eh.produceError(ErrorHandling.ErrorType.Success);
    assertEq(message, "success");
  }

  function testErrorHandlingExplicitAssert() public {
    // * manual assertion
    try eh.produceError(ErrorHandling.ErrorType.ExplicitAssert)
      returns (string memory){
      revert ErrExpectedErrorGotNone();
    } catch Panic(uint code) {
      assertEq(code, 1);
    }
    // * forge assertion
    vm.expectRevert(stdError.assertionError);
    eh.produceError(ErrorHandling.ErrorType.ExplicitAssert);
  }

  function testErrorHandlingImplicitPanic() public {
    // * manual assertion
    try eh.produceError(ErrorHandling.ErrorType.ImplicitPanic)
      returns (string memory) {
      revert ErrExpectedErrorGotNone();
    } catch Panic(uint code) {
      assertEq(code, 17);
    }
    // * forge assertion
    vm.expectRevert(stdError.arithmeticError);
    eh.produceError(ErrorHandling.ErrorType.ImplicitPanic);
  }

  function testErrorHandlingRevertWithCustomError() public {
    // * manual assertion
    try eh.produceError(ErrorHandling.ErrorType.Revert)
      returns (string memory) {
      revert ErrExpectedErrorGotNone();
    } catch (bytes memory err) { // custom errors must be handled as bytes
      bytes4 expSelector = ErrorHandling.ErrOh.selector;
      bytes4 gotSelector = bytes4(err);
      assertEq(gotSelector, expSelector);
      bytes memory expErr = abi.encodeWithSignature(
        "ErrOh(string)", "revert error"
      );
      assertEq(err, expErr);
    }
    // * forge assertion
    vm.expectRevert(abi.encodeWithSignature(
      "ErrOh(string)", "revert error"
    ));
    eh.produceError(ErrorHandling.ErrorType.Revert);
  }

  function testErrorHandlingRequireWithError() public {
    // * manual assertion
    try eh.produceError(ErrorHandling.ErrorType.Require)
      returns (string memory) {
      revert ErrExpectedErrorGotNone();
    } catch Error(string memory message) {
      assertEq(message, "require error");
    }
    // * forge assertion
    vm.expectRevert("require error");
    eh.produceError(ErrorHandling.ErrorType.Require);
  }
}
