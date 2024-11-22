// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {CrowdFunding} from "contract/CrowdFunding.sol";

contract CrowdFundingTest is Test {
  CrowdFunding fund;
  address payable owner;
  uint campaignID;
  address payable plg1;
  address payable plg2;
  uint value1;
  uint value2;

  function setUp() public {
    fund = new CrowdFunding();
    owner = payable(makeAddr("owner"));
    (plg1, plg2) = (payable(makeAddr("plg1")), payable(makeAddr("plg2")));
    // Launch campaign
    uint goal = 10 ether;
    vm.expectEmit(false, true, false, false);
    emit CrowdFunding.EvLaunch(0, owner, goal);
    vm.prank(owner);
    campaignID = fund.launch(goal);
  }

  function testPledgeMoreThenClaim() public {
    // Pledge to reach the goal
    (value1, value2) = (5 ether, 6 ether);
    vm.expectEmit(true, true, false, true);
    emit CrowdFunding.EvPledge(campaignID, plg1, value1);
    hoax(plg1, value1);
    fund.pledge{value: value1}(campaignID);
    vm.expectEmit(true, true, false, true);
    emit CrowdFunding.EvPledge(campaignID, plg2, value2);
    hoax(plg2, value2);
    fund.pledge{value: value2}(campaignID);
    assertEq(address(fund).balance, value1 + value2);
    // End the campaign
    bool goalReached = true;
    vm.expectEmit(true, false, false, true);
    emit CrowdFunding.EvEnd(campaignID, goalReached);
    vm.prank(owner);
    fund.end(campaignID);
    // Claim the pledged total
    vm.expectEmit(true, true, false, true);
    emit CrowdFunding.EvClaim(campaignID, owner, value1 + value2);
    vm.prank(owner);
    fund.claim(campaignID);
    assertEq(owner.balance, value1 + value2);
    // Claim again => ErrClaimedCampaign(true)
    bytes memory err = abi.encodeWithSelector(
      CrowdFunding.ErrClaimedCampaign.selector, true, campaignID
    );
    vm.expectRevert(err);
    vm.prank(owner);
    fund.claim(campaignID);
  }

  function testPledgeLessThenWithdraw() public {
    // Pledge not enough to reach the goal
    (value1, value2) = (3 ether, 4 ether);
    vm.expectEmit(true, true, false, true);
    emit CrowdFunding.EvPledge(campaignID, plg1, value1);
    hoax(plg1, value1);
    fund.pledge{value: value1}(campaignID);
    vm.expectEmit(true, true, false, true);
    emit CrowdFunding.EvPledge(campaignID, plg2, value2);
    hoax(plg2, value2);
    fund.pledge{value: value2}(campaignID);
    assertEq(address(fund).balance, value1 + value2);
    // End the campaign
    bool goalReached = false;
    vm.expectEmit(true, false, false, true);
    emit CrowdFunding.EvEnd(campaignID, goalReached);
    vm.prank(owner);
    fund.end(campaignID);
    // Claim when the goal is not reached => ErrGoalReached(false)
    bytes memory err = abi.encodeWithSelector(
      CrowdFunding.ErrGoalReached.selector, goalReached, campaignID
    );
    vm.expectRevert(err);
    vm.prank(owner);
    fund.claim(campaignID);
    // Withdraw pledged value
    vm.expectEmit(true, true, false, true);
    emit CrowdFunding.EvWithdraw(campaignID, plg1, value1);
    vm.prank(plg1);
    fund.withdraw(campaignID);
    assertEq(plg1.balance, value1);
    vm.expectEmit(true, true, false, true);
    emit CrowdFunding.EvWithdraw(campaignID, plg2, value2);
    vm.prank(plg2);
    fund.withdraw(campaignID);
    assertEq(plg2.balance, value2);
  }
}
