// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {console} from "forge-std/console.sol";

contract Send {
  function sendTransfer(address payable to) public payable {
    console.log("==> transfer %s", msg.value);
    to.transfer(msg.value); // not recommended
  }

  function sendSend(address payable to) public payable {
    console.log("==> send %s", msg.value);
    bool success = to.send(msg.value); // not recommended
    require(success, "send failure");
  }

  function sendCall(address payable to) public payable {
    console.log("==> call %s", msg.value);
    (bool success, ) = to.call{value: msg.value}(""); // recommended
    require(success, "call failure");
  }

  function sendDeposit(address payable to) public payable {
    console.log("==> deposit %s", msg.value);
    Receive(to).deposit{value: msg.value}();
  }
}

contract Receive {
  event EvReceive(address indexed from, uint value);

  function deposit() external payable {
    console.log("<== deposit %s", msg.value);
    emit EvReceive(msg.sender, msg.value);
  }

  receive() external payable {
    console.log("<== receive %s", msg.value);
    emit EvReceive(msg.sender, msg.value);
  }

  fallback() external payable {
    console.log("<== fallback %s", msg.value);
    emit EvReceive(msg.sender, msg.value);
  }
}
