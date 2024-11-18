// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {console} from "forge-std/console.sol";

contract MultiSigWallet {
  struct Tx {
    address to;
    uint value;
    bytes data;
    address[] approvals;
    bool executed;
  }

  address[] public ownerList;
  mapping(address => bool) public ownerMap;
  uint public minApprovals;
  Tx[] public txs;

  event EvReceive(address indexed from, uint value);
  event EvTxPropose(uint indexed txi);
  event EvTxApprove(
    uint indexed txi, address indexed approver, uint approvals, uint minApprovals
  );
  event EvTxRevokeApproval(
    uint indexed txi, address indexed revoker, uint approvals, uint minApprovals
  );
  event EvTxExecute(uint indexed txi, address indexed executor);

  error ErrEmptyOwnerList();
  error ErrDuplicateOwner(address owner);
  error ErrInvalidMinApprovals(uint minApproval);
  error ErrUnauthorized(address account);
  error ErrTxNotFound(uint txi);
  error ErrTxNotApproved(uint txi);
  error ErrTxAlreadyApproved(uint txi);
  error ErrTxNotExecuted(uint txi);
  error ErrTxAlreadyExecuted(uint txi);
  error ErrDuplicateApprover(uint txi, address approver);
  error ErrRevokerNotFound(uint txi, address revoker);
  error ErrTxExecute(uint txi);

  modifier onlyOwners() {
    require(ownerMap[msg.sender], ErrUnauthorized(msg.sender));
    _;
  }

  modifier existsTx(uint txi) {
    require(txi < txs.length, ErrTxNotFound(txi));
    _;
  }

  modifier approvedTx(bool approved, uint txi) {
    if (approved) {
      require(txs[txi].approvals.length >= minApprovals, ErrTxNotApproved(txi));
    } else {
      require(txs[txi].approvals.length < minApprovals, ErrTxAlreadyApproved(txi));
    }
    _;
  }

  modifier executedTx(bool executed, uint txi) {
    if (executed) {
      require(txs[txi].executed, ErrTxNotExecuted(txi));
    } else {
      require(!txs[txi].executed, ErrTxAlreadyExecuted(txi));
    }
    _;
  }

  constructor(address[] memory owners, uint minAppr) {
    require(owners.length > 0, ErrEmptyOwnerList());
    require(minAppr <= owners.length, ErrInvalidMinApprovals(minAppr));
    for (uint i = 0; i < owners.length; i++) {
      address owner = owners[i];
      require(!ownerMap[owner], ErrDuplicateOwner(owner));
      ownerList.push(owner);
      ownerMap[owner] = true;
    }
    minApprovals = minAppr;
  }

  receive() external payable {
    emit EvReceive(msg.sender, msg.value);
  }

  function proposeTx(address to, uint value, bytes memory data)
    public onlyOwners {
    uint txi = txs.length;
    Tx memory txn = Tx({
      to: to, value: value, data: data,
      approvals: new address[](0), executed: false
    });
    txs.push(txn);
    emit EvTxPropose(txi);
  }

  function approveTx(uint txi) public onlyOwners existsTx(txi)
    approvedTx(false, txi) executedTx(false, txi) {
    address approver = msg.sender;
    Tx storage txn = txs[txi];
    for (uint i = 0; i < txn.approvals.length; i++) {
      require(txn.approvals[i] != approver, ErrDuplicateApprover(txi, approver));
    }
    txn.approvals.push(approver);
    emit EvTxApprove(txi, approver, txn.approvals.length, minApprovals);
  }

  function revokeTxApproval(uint txi) public onlyOwners existsTx(txi)
    executedTx(false, txi) {
    address revoker = msg.sender;
    Tx storage txn = txs[txi];
    uint len = txn.approvals.length;
    for (uint i = 0; i < len; i++) {
      if (txn.approvals[i] == revoker) {
        txn.approvals[i] = txn.approvals[len - 1];
        txn.approvals.pop();
        emit EvTxRevokeApproval(txi, revoker, txn.approvals.length, minApprovals);
        return;
      }
    }
    revert ErrRevokerNotFound(txi, revoker);
  }

  function executeTx(uint txi) public onlyOwners existsTx(txi)
    approvedTx(true, txi) executedTx(false, txi) {
    Tx storage txn = txs[txi];
    txn.executed = true;
    (bool success, ) = txn.to.call{value: txn.value}(txn.data);
    if (!success) {
      txn.executed = false;
      revert ErrTxExecute(txi);
    }
    emit EvTxExecute(txi, msg.sender);
  }
}
