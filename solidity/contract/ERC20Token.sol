// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

interface ERC20 {
  event Transfer(address indexed from, address indexed to, uint value);
  event Approval(address indexed owner, address indexed spender, uint maxValue);

  function totalSupply() external view returns (uint totalSupply);
  function balanceOf(address owner) external view returns (uint balance);
  function transfer(address to, uint value) external returns (bool success);
  function approve(address spender, uint maxValue)
    external returns (bool success);
  function allowance(address owner, address spender)
    external view returns (uint remainingValue);
  function transferFrom(address from, address to, uint value)
    external returns (bool success);
}

contract ERC20Token is ERC20 {
  uint public totalSupply;
  // owner => balance
  mapping(address => uint) public balanceOf;
  // owner => spender => remainingValue
  mapping(address => mapping(address => uint)) public allowance;

  error ErrInsufficientFunds(address sender, uint value);
  error ErrBeyondAllowance(address owner, address spender, uint value);

  modifier hasFunds(address owner, uint value) {
    require(
      balanceOf[owner] >= value, ErrInsufficientFunds(owner, value)
    );
    _;
  }

  modifier withinAllowance(address owner, address spender, uint value) {
    require(
      value <= allowance[owner][spender],
      ErrBeyondAllowance(owner, spender, value)
    );
    _;
  }

  constructor(address owner, uint totSupp) {
    totalSupply = totSupp;
    balanceOf[owner] = totalSupply;
  }

  function transfer(address to, uint value)
    public hasFunds(msg.sender, value) returns (bool success) {
    balanceOf[msg.sender] -= value;
    balanceOf[to] += value;
    emit Transfer(msg.sender, to, value);
    return true;
  }

  function approve(address spender, uint maxValue)
    public hasFunds(msg.sender, maxValue) returns (bool success) {
    allowance[msg.sender][spender] = maxValue;
    emit Approval(msg.sender, spender, maxValue);
    return true;
  }

  function transferFrom(address from, address to, uint value)
    external hasFunds(from, value) withinAllowance(from, msg.sender, value)
    returns (bool success) {
    allowance[from][msg.sender] -= value;
    balanceOf[from] -= value;
    balanceOf[to] += value;
    emit Transfer(from, to, value);
    return true;
  }
}
