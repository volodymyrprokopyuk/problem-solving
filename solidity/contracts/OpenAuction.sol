// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract OpenAuction {
  address payable public beneficiary;
  uint256 public endTime;
  address public topBidder;
  uint256 public topBid;

  mapping(address => uint256) pendingReturns;
  bool auctionEnded;

  event EvNewTopBid(address bidder, uint256 bid);
  event EvWithdraw(address bidder, uint256 bid);
  event EvAuctionEnd(address topBidder, uint256 topBid);

  error ErrNotTopBid(uint256 topBid);
  error ErrNothingToReturn();
  error ErrWithdraw();
  error ErrEarlyToEnd();
  error ErrAuctionEnded();

  constructor(uint256 durationSecs, address payable benef) {
    beneficiary = benef;
    endTime = block.timestamp + durationSecs;
  }

  function bid() external payable {
    if (block.timestamp > endTime) {
      revert ErrAuctionEnded();
    }
    if (msg.value <= topBid) {
      revert ErrNotTopBid(topBid);
    }
    if (topBid != 0) {
      pendingReturns[topBidder] += topBid;
    }
    topBidder = msg.sender;
    topBid = msg.value;
    emit EvNewTopBid(topBidder, topBid);
  }

  function withdraw() external {
    uint256 returnBid = pendingReturns[msg.sender];
    require(returnBid > 0, ErrNothingToReturn());
    pendingReturns[msg.sender] = 0;
    if (!payable(msg.sender).send(returnBid)) {
      pendingReturns[msg.sender] = returnBid;
      revert ErrWithdraw();
    }
    emit EvWithdraw(msg.sender, returnBid);
  }

  function endAuction() external {
    // if (block.timestamp < endTime) {
    //   revert ErrEarlyToEnd();
    // }
    if (auctionEnded) {
      revert ErrAuctionEnded();
    }
    auctionEnded = true;
    emit EvAuctionEnd(topBidder, topBid);
    beneficiary.transfer(topBid);
  }
}
