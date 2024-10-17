// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract Token {
  string public name = "Token";
  string public symbol = "TOK";
  address public owner;
  uint256 public totalSupply = 1000;
  mapping(address => uint256) balances;
  event Transfer(address indexed from, address indexed to, uint256 value);

  constructor() {
    owner = msg.sender;
    balances[owner] = totalSupply;
  }

  function transfer(address to, uint256 value) external {
    require(balances[msg.sender] >= value, "insufficient funds");
    balances[msg.sender] -= value;
    balances[to] += value;
    emit Transfer(msg.sender, to, value);
  }

  function balance(address account) external view returns (uint256) {
    return balances[account];
  }
}
