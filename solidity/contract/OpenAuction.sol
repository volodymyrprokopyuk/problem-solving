// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract OpenAuction {
  address payable owner;
  uint topBid;
  address topBidder;
  mapping(address => uint) refunds;
  bool closed;

  event EvTopBid(uint topBid);
  event EvClose();
  event EvWithdraw(uint value);

  error ErrUnauthorized(address account);
  error ErrAlreadyClosed();
  error ErrStillOpen();
  error ErrNotTopBid(uint topBid);
  error ErrNothingToRefund(address account);
  error ErrSendFailed(string action);

  modifier only(address target) {
    require(msg.sender == target, ErrUnauthorized(msg.sender));
    _;
  }

  modifier isOpen(bool open) {
    if (open && closed) {
      revert ErrAlreadyClosed();
    }
    if (!open && !closed) {
      revert ErrStillOpen();
    }
    _;
  }

  constructor() {
    owner = payable(msg.sender);
  }

  function bid() external payable isOpen(true) {
    require(msg.value > topBid, ErrNotTopBid(topBid));
    if (topBid != 0) {
      refunds[topBidder] += topBid;
    }
    topBid = msg.value;
    topBidder = msg.sender;
    emit EvTopBid(topBid);
  }

  function close() external only(owner) isOpen(true) {
    closed = true;
    emit EvClose();
    (bool success, ) = owner.call{value: topBid}("");
    require(success, ErrSendFailed("close"));
  }

  function withdraw() external isOpen(false) {
    address bidder = msg.sender;
    uint value = refunds[bidder];
    refunds[bidder] = 0;
    require(value > 0, ErrNothingToRefund(bidder));
    (bool success, ) = bidder.call{value: value}("");
    require(success, ErrSendFailed("withdraw"));
    emit EvWithdraw(value);
  }
}
