// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract BlindAuction {
  struct Bid {
    bytes32 blindBid;
    uint256 deposit;
  }

  mapping(address => Bid[]) public bids;
  address public topBidder;
  uint256 public topBid;
  address payable public beneficiary;

  mapping(address => uint256) pendingReturns;
  bool auctionEnded;

  event EvNewTopBid(address bidder, uint256 bid);
  event EvWithdraw(address bidder, uint256 bid);
  event EvAuctionEnd(address topBidder, uint256 topBid);

  error ErrInvalidReveal();
  error ErrNothingToReturn();
  error ErrWithdraw();
  error ErrAuctionEnded();

  modifier onlyBeforeAuctionEnd() {
    require(!auctionEnded, ErrAuctionEnded());
    _;
  }

  constructor(address payable benef) {
    beneficiary = benef;
  }

  function placeBid(bytes32 blindBid) external payable onlyBeforeAuctionEnd() {
    Bid memory bid = Bid({ blindBid: blindBid, deposit: msg.value });
    bids[msg.sender].push(bid);
  }

  function reveal(uint256[] calldata values) external {
    Bid[] memory senderBids = bids[msg.sender];
    require(senderBids.length == values.length, ErrInvalidReveal());
    for (uint256 i = 0; i < senderBids.length; i++) {
      Bid memory senderBid = senderBids[i];
      uint256 revealValue = values[i];
      if (senderBid.blindBid != keccak256(abi.encodePacked(revealValue))) {
        revert ErrInvalidReveal();
      }
      if (!checkTopBid(msg.sender, revealValue)) {
        pendingReturns[msg.sender] += revealValue;
      }
      senderBid.blindBid = bytes32(0);
    }
  }

  function checkTopBid(address bidder, uint256 bid) internal returns (bool) {
    if (bid <= topBid) {
      return false;
    }
    if (topBidder != address(0)) {
      pendingReturns[topBidder] += topBid;
    }
    topBidder = bidder;
    topBid = bid;
    emit EvNewTopBid(bidder, bid);
    return true;
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
    if (auctionEnded) {
      revert ErrAuctionEnded();
    }
    auctionEnded = true;
    emit EvAuctionEnd(topBidder, topBid);
    beneficiary.transfer(topBid);
  }
}
