// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract Token {
  address private owner;
  mapping(address => uint) public balances;

  event EvTransfer(address indexed from, address indexed to, uint value);

  error ErrInsufficientFunds(uint required);

  constructor(uint supply) {
    owner = msg.sender;
    balances[owner] = supply;
  }

  function transfer(address to) public payable {
    (address from, uint value) = (msg.sender, msg.value);
    require(balances[from] >= value, ErrInsufficientFunds(value));
    balances[from] -= value;
    balances[to] += value;
    emit EvTransfer(from, to, value);
  }
}
