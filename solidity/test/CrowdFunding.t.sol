// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {CrowdFunding} from "contract/CrowdFunding.sol";

contract CrowdFundingTest is Test {
  CrowdFunding fund;
  address payable owner;
  address payable plg1;
  address payable plg2;

  function setUp() public {
    fund = new CrowdFunding();
    owner = payable(makeAddr("owner"));
    (plg1, plg2) = (payable(makeAddr("plg1")), payable(makeAddr("plg2")));
  }

  function testLaunchPledgeEndClaimWithdraw() public {
    uint goal = 10 ether;
    // vm.expectEmit(true, true, false, true);
    // emit CrowdFunding.EvLaunch(1, owner, goal);
    vm.prank(owner);
    // uint campaignID = fund.launch(goal);
    fund.launch(goal);
  }
}
