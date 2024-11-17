// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract BlindAuction {
  address payable owner;
  mapping(address => bytes32[]) hashBids;
  uint topBid;
  address topBidder;
  mapping(address => uint) refunds;
  bool closed;

  event EvTopBid(uint topBid);
  event EvClose();
  event EvWithdraw(uint value);

  error ErrUnauthorized(address account);
  error ErrAlreadyClsoed();
  error ErrStillOpen();
  error ErrNothingToReveal(address account);
  error ErrInvalidBid(address bidder, uint bid);
  error ErrSendFailed(string action);
  error ErrNothingToRefund(address account);

  modifier only(address target) {
    require(msg.sender == target, ErrUnauthorized(msg.sender));
    _;
  }

  modifier isOpen(bool open) {
    if (open && closed) {
      revert ErrAlreadyClsoed();
    }
    if (!open && !closed) {
      revert ErrStillOpen();
    }
    _;
  }

  constructor() {
    owner = payable(msg.sender);
  }

  function bid(bytes32 hashBid) external payable isOpen(true) {
    hashBids[msg.sender].push(hashBid);
  }

  function reveal(uint[] memory bids) external {
    address bidder = msg.sender;
    bytes32[] storage hashBds = hashBids[bidder];
    require(hashBds.length > 0, ErrNothingToReveal(bidder));
    for (uint i = 0; i < hashBds.length; i++) {
      uint bd = bids[i];
      require(
        hashBds[i] == keccak256(abi.encode(bd)), ErrInvalidBid(bidder, bd)
      );
      if (bd > topBid) {
        if (topBid != 0) {
          refunds[topBidder] += topBid;
        }
        topBid = bd;
        topBidder = bidder;
        emit EvTopBid(bd);
      } else {
        refunds[bidder] += bd;
      }
      hashBds[i] = bytes32(0);
    }
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
    require(value > 0, ErrNothingToRefund(bidder));
    (bool success, ) = bidder.call{value: value}("");
    require(success, ErrSendFailed("withdraw"));
    emit EvWithdraw(value);
  }
}
