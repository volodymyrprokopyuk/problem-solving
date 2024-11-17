// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {BlindAuction} from "contract/BlindAuction.sol";

contract BlindAuctionTest is Test {
  BlindAuction auct;
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
    auct = new BlindAuction();
  }

  function testBidRevealCloseWithdraw() public {
    // Bid 1 ether
    uint value1 = 1 ether;
    hoax(bd1, value1);
    auct.bid{value: value1}(keccak256(abi.encode(value1)));
    // Bid 3 ether
    uint value3 = 3 ether;
    hoax(bd1, value3);
    auct.bid{value: value3}(keccak256(abi.encode(value3)));
    // Bid 2 ether
    uint value2 = 2 ether;
    hoax(bd2, value2);
    auct.bid{value: value2}(keccak256(abi.encode(value2)));
    assertEq(address(auct).balance, value1 + value2 + value3);
    // Reveal 1, 3
    vm.expectEmit(true, false, false, true);
    emit BlindAuction.EvTopBid(value3);
    uint[] memory bids2 = new uint[](2);
    (bids2[0], bids2[1]) = (value1, value3);
    vm.prank(bd1);
    auct.reveal(bids2);
    // Reveal 10 => ErrInvalidBid
    uint[] memory bids1 = new uint[](1);
    uint value10 = 10 ether;
    bids1[0] = value10;
    bytes memory err = abi.encodeWithSelector(
      BlindAuction.ErrInvalidBid.selector, bd2, value10
    );
    vm.expectRevert(err);
    vm.prank(bd2);
    auct.reveal(bids1);
    // Reveal 2
    bids1[0] = value2;
    vm.prank(bd2);
    auct.reveal(bids1);
    // Close
    vm.prank(owner);
    auct.close();
    assertEq(owner.balance, value3);
    // Withdraw
    vm.expectEmit(true, false, false, true);
    emit BlindAuction.EvWithdraw(value1);
    vm.prank(bd1);
    auct.withdraw();
    assertEq(bd1.balance, value1);
    vm.expectEmit(true, false, false, true);
    emit BlindAuction.EvWithdraw(value2);
    vm.prank(bd2);
    auct.withdraw();
    assertEq(bd2.balance, value2);
    assertEq(address(auct).balance, 0);
  }
}
