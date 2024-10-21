// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract SimpleCoin {
  address public minter;
  mapping(address => uint256) public balances;

  event Sent(address from, address to, uint256 value);

  constructor () {
    minter = msg.sender;
  }

  function mint(address to, uint256 value) public {
    require(msg.sender == minter);
    balances[to] += value;
  }

  error InsufficientFunds(uint256 requested, uint256 available);

  function send(address to, uint256 value) public {
    uint256 senderBalance = balances[msg.sender];
    require(senderBalance >= value, InsufficientFunds(value, senderBalance));
    balances[msg.sender] -= value;
    balances[to] += value;
    emit Sent(msg.sender, to, value);
  }
}
