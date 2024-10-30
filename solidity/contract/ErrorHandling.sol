// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract ErrorHandling {
  enum ErrorType { Success, ExplicitAssert, ImplicitPanic, Revert, Require }

  error ErrOh(string message);

  function produceError(ErrorType errorType) public pure
    returns (string memory) {
    if (errorType == ErrorType.ExplicitAssert) {
      assert(errorType == ErrorType.Success);
    } else if (errorType == ErrorType.ImplicitPanic) {
      uint overflow = type(uint).max + 1;
      require(overflow == 0); // use overflow
    } else if (errorType == ErrorType.Revert) {
      revert ErrOh("revert error");
    } else if (errorType == ErrorType.Require) {
      require(errorType == ErrorType.Success, "require error");
    }
    return "success";
  }
}
