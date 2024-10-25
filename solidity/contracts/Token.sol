// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import "hardhat/console.sol";

library Balances {
  error ErrInsufficientFunds();

  function transfer(
    mapping(address => uint256) storage balances,
    address from, address to, uint256 value
  ) internal {
    require(balances[from] >= value, ErrInsufficientFunds());
    require(balances[to] + value > balances[to]);
    balances[from] -= value;
    balances[to] += value;
  }
}

contract Token {
  using Balances for *;
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
    // require(balances[msg.sender] >= value, "insufficient funds");
    // balances[msg.sender] -= value;
    // balances[to] += value;
    balances.transfer(msg.sender, to, value);
    console.log("transfer: %s => %s %s", msg.sender, to, value);
    emit Transfer(msg.sender, to, value);
  }

  function balance(address account) external view returns (uint256) {
    return balances[account];
  }
}
