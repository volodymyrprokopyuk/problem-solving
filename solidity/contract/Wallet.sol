// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract Wallet {
  address payable public owner;

  error ErrUnauthorized(address account);
  error ErrInsufficientFunds(uint required);
  error ErrSendFailed(string action);

  modifier only(address target) {
    require(msg.sender == target, ErrUnauthorized(msg.sender));
    _;
  }

  constructor() {
    owner = payable(msg.sender);
  }

  receive() external payable { }

  function balance() external view returns (uint) {
    return address(this).balance;
  }

  function withdraw(uint value) external only(owner) {
    require(value <= address(this).balance, ErrInsufficientFunds(value));
    (bool success, ) = owner.call{value: value}("");
    require(success, ErrSendFailed("withdraw"));
  }
}
