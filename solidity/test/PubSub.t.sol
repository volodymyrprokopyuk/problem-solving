// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {Publisher, Subscriber} from "contract/PubSub.sol";

contract PubSubTest is Test {
  Publisher pub;
  Subscriber sub1;
  Subscriber sub2;

  function setUp() public {
    pub = new Publisher();
    (sub1, sub2) = (new Subscriber(), new Subscriber());
  }

  function testSubscribeUnsubscribePublish() public {
    vm.prank(address(sub1));
    pub.subscribe();
    vm.prank(address(sub2));
    pub.subscribe();
    pub.publish("msg1");
    vm.prank(address(sub2));
    pub.unsubscribe();
    pub.publish("msg2");
    assertEq(sub1.messages(0), "msg1");
    assertEq(sub1.messages(1), "msg2");
    assertEq(sub2.messages(0), "msg1");
  }
}
