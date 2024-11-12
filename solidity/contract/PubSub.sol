// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

interface ISubscriber {
  function handle(string memory message) external;
}

contract Publisher {
  // For efficient lookup on subscribe() and unsubscribe()
  mapping(address => bool) subMap;
  // For enumeration on publish()
  address[] subList;

  error ErrAlreadySubscribed(address sub);
  error ErrNotSubscribed(address sub);

  function subscribe() public {
    require(!subMap[msg.sender], ErrAlreadySubscribed(msg.sender));
    subMap[msg.sender] = true;
    subList.push(msg.sender);
  }

  function unsubscribe() public {
    require(subMap[msg.sender], ErrNotSubscribed(msg.sender));
    delete subMap[msg.sender];
    for (uint i = 0; i < subList.length; i++) {
      if (subList[i] == msg.sender) {
        subList[i] = subList[subList.length - 1];
        subList.pop();
        break;
      }
    }
  }

  function publish(string memory message) public {
    for (uint i = 0; i < subList.length; i++) {
      ISubscriber(subList[i]).handle(message);
    }
  }
}

contract Subscriber {
  string[] public messages;

  function handle(string memory message) public {
    messages.push(message);
  }
}
