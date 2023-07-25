//SPDX-License-Identifier: UNLICENSED

pragma solidity ^0.8.0;

contract Token {
  string public tokenName = "Hardhat token";
  string public tokenSymbol = "HHT";
  uint256 public totalSupply = 1e6;
  address public owner;
  mapping(address => uint256) balances;

  event Transfer(address indexed _from, address indexed _to, uint256 _value);

  constructor() {
    owner = msg.sender;
    balances[owner] = totalSupply;
  }

  function transfer(address to, uint256 value) external {
    require(balances[msg.sender] >= value, "Insufficient balance");
    balances[msg.sender] -= value;
    balances[to] += value;
    emit Transfer(msg.sender, to, value);
  }

  function balanceOf(address account) external view returns (uint256) {
    return balances[account];
  }
}
