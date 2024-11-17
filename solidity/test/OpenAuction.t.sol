// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {OpenAuction} from "contract/OpenAuction.sol";

contract OpenAuctionTest is Test {
  OpenAuction auct;
  address payable owner;
  address payable bd1;
  address payable bd2;

  function setUp() public {
    (owner, bd1, bd2) =
      (payable(makeAddr("owner")),
       payable(makeAddr("bd1")),
       payable(makeAddr("bd2"))
      );
    vm.prank(owner);
    auct = new OpenAuction();
  }

  function testBidCloseWithdraw() public {
    // Bid 1 ether
    uint value1 = 1 ether;
    vm.expectEmit(true, false, false, true);
    emit OpenAuction.EvTopBid(value1);
    hoax(bd1, value1);
    auct.bid{value: value1}();
    // Bid 1 ether
    uint value2 = 2 ether;
    vm.expectEmit(true, false, false, true);
    emit OpenAuction.EvTopBid(value2);
    hoax(bd2, value2);
    auct.bid{value: value2}();
    assertEq(address(auct).balance, value1 + value2);
    // Withdraw before close => ErrStillOpen
    vm.expectRevert(OpenAuction.ErrStillOpen.selector);
    vm.prank(bd1);
    auct.withdraw();
    // Close
    vm.expectEmit(true, false, false, true);
    emit OpenAuction.EvClose();
    vm.prank(owner);
    auct.close();
    assertEq(owner.balance, value2);
    // Withdraw top bid => ErrNothingToRefund
    vm.expectPartialRevert(OpenAuction.ErrNothingToRefund.selector);
    vm.prank(bd2);
    auct.withdraw();
    // Withdraw not a top bid
    vm.expectEmit(true, false, false, true);
    emit OpenAuction.EvWithdraw(value1);
    vm.prank(bd1);
    auct.withdraw();
    assertEq(bd1.balance, value1);
    assertEq(address(auct).balance, 0);
  }
}
