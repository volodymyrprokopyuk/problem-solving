// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

contract Counter {
  address public immutable owner;
  uint256 public number;

  event EvIncrement(uint256 indexed number);

  error ErrOh();
  error ErrOnlyOwner();

  constructor() {
    owner = msg.sender;
  }

  function setNumber(uint256 newNumber) public {
    require(msg.sender == owner, ErrOnlyOwner());
    number = newNumber;
  }

  function increment() public {
    require(msg.sender == owner, ErrOnlyOwner());
    number++;
    emit EvIncrement(number);
  }

  function doRevert() pure public {
    revert ErrOh();
  }
}
