// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

// ERC-20 Token standard (tokens are indistinguishable, identical)
interface IERC20 {
  // On successful transfer() and transferFrom()
  event Transfer(address indexed from, address indexed to, uint value);
  // On approve()
  event Approval(address indexed owner, address indexed spender, uint maxValue);

  function totalSupply() external view returns (uint totalSupply);
  function balanceOf(address owner) external view returns (uint balance);
  // The owner == msg.sender transfers the value from the owner's account
  function transfer(address to, uint value) external returns (bool success);
  // The owner == msg.sender approves a spender to transfer up to the maxValue
  // on the owner's behalf
  function approve(address spender, uint maxValue)
    external returns (bool success);
  // Checks the remaining value approved by the owner for the spender to
  // transfer on the owner's behalf
  function allowance(address owner, address spender)
    external view returns (uint remainingValue);
  // The spender == msg.sender transfers the value on the owners behalf
  function transferFrom(address from, address to, uint value)
    external returns (bool success);
}

contract ERC20Token is IERC20 {
  uint public totalSupply;
  // owner => balance
  mapping(address => uint) public balanceOf;
  // owner => spender => remainingValue
  mapping(address => mapping(address => uint)) public allowance;

  error ErrInsufficientFunds(address owner, uint value);
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

contract ERC20TokenSwap {
  IERC20 tokenA;
  IERC20 tokenB;

  error ErrInsufficientFunds(address token, address owner, uint value);
  error ErrBeyondAllowance(address owner, address spender, uint value);
  error ErrTransfer(address from, address to, uint value);

  constructor(address tokA, address tokB) {
    tokenA = IERC20(tokA);
    tokenB = IERC20(tokB);
  }

  function swap(
    address ownerA, uint valueA, address ownerB, uint valueB
  ) public {
    address swapper = address(this);
    require(
      tokenA.balanceOf(ownerA) >= valueA,
      ErrInsufficientFunds(address(tokenA), ownerA, valueA)
    );
    require(
      tokenA.allowance(ownerA, swapper) >= valueA,
      ErrBeyondAllowance(ownerA, swapper, valueA)
    );
    require(
      tokenB.balanceOf(ownerB) >= valueB,
      ErrInsufficientFunds(address(tokenB), ownerB, valueB)
    );
    require(
      tokenB.allowance(ownerB, swapper) >= valueB,
      ErrBeyondAllowance(ownerB, swapper, valueB)
    );
    bool success = tokenA.transferFrom(ownerA, ownerB, valueA);
    require(success, ErrTransfer(ownerA, ownerB, valueA));
    success = tokenB.transferFrom(ownerB, ownerA, valueB);
    require(success, ErrTransfer(ownerB, ownerA, valueB));
  }
}
