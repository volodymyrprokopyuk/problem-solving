// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {MultiSigWallet} from "contract/MultiSigWallet.sol";

contract MultiSigWalletTest is Test {
  address owner1;
  address owner2;
  address owner3;
  address[] owners;
  address payable benef;
  uint minApprovals = 2;
  MultiSigWallet wallet;
  uint funds = 10 ether;

  function setUp() public {
    (owner1, owner2, owner3) =
      (makeAddr("owner1"), makeAddr("owner2"), makeAddr("owner3"));
    benef = payable(makeAddr("benef"));
    owners.push(owner1); owners.push(owner2); owners.push(owner3);
    wallet = new MultiSigWallet(owners, minApprovals);
    vm.expectEmit(true, false, false, true);
    emit MultiSigWallet.EvReceive(address(this), funds);
    (bool success, ) = address(wallet).call{value: funds}("");
    assertTrue(success);
    assertEq(address(wallet).balance, funds);
  }

  function testProposeApproveExecute() public {
    // Propose
    uint txi = 0;
    vm.expectEmit(true, false, false, true);
    emit MultiSigWallet.EvTxPropose(txi);
    vm.prank(owner1);
    (uint value, bytes memory data) = (1 ether, "payment");
    wallet.proposeTx(benef, value, data);
    // Approve +1
    uint approvals = 1;
    vm.expectEmit(true, true, false, true);
    emit MultiSigWallet.EvTxApprove(txi, owner1, approvals, minApprovals);
    vm.prank(owner1);
    wallet.approveTx(txi);
    // Revoke TX approval -1
    approvals--;
    vm.expectEmit(true, true, false, true);
    emit MultiSigWallet.EvTxRevokeApproval(txi, owner1, approvals, minApprovals);
    vm.prank(owner1);
    wallet.revokeTxApproval(txi);
    // Approve +1
    approvals++;
    vm.expectEmit(true, true, false, true);
    emit MultiSigWallet.EvTxApprove(txi, owner2, approvals, minApprovals);
    vm.prank(owner2);
    wallet.approveTx(txi);
    // Approve +1
    approvals++;
    vm.expectEmit(true, true, false, true);
    emit MultiSigWallet.EvTxApprove(txi, owner3, approvals, minApprovals);
    vm.prank(owner3);
    wallet.approveTx(txi);
    // Execute
    vm.expectEmit(true, true, false, true);
    emit MultiSigWallet.EvTxExecute(txi, owner1);
    vm.prank(owner1);
    wallet.executeTx(txi);
    assertEq(benef.balance, value);
    assertEq(address(wallet).balance, funds - value);
    // Revoke => ErrTxAlreadyExecuted
    bytes memory err = abi.encodeWithSelector(
      MultiSigWallet.ErrTxAlreadyExecuted.selector, txi
    );
    vm.expectRevert(err);
    vm.prank(owner1);
    wallet.revokeTxApproval(txi);
    // Execute => ErrTxAlreadyExecuted
    vm.expectRevert(err);
    vm.prank(owner1);
    wallet.executeTx(txi);
  }
}
