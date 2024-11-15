// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract SafePurchase {
  enum State { Created, Canceled, Locked, Received, Refunded }

  address payable private seller;
  address payable private buyer;
  State private state;
  uint public price;

  event EvOffer(uint sellerValue);
  event EvCancel(uint sellerValue);
  event EvPurchase(uint buyerValue);
  event EvReceive(uint buyerValue);
  event EvRefund(uint sellerValue);

  error ErrNotEvenValue(uint value);
  error ErrUnauthorized(address account);
  error ErrWrongState(State state);
  error ErrSendFailed(string operation);

  modifier only(address target) {
    require(msg.sender == target, ErrUnauthorized(msg.sender));
    _;
  }

  modifier inState(State target) {
    require(state == target, ErrWrongState(state));
    _;
  }

  constructor() payable {
    seller = payable(msg.sender);
    state = State.Created;
    price = msg.value / 2;
    require((price * 2) == msg.value, ErrNotEvenValue(msg.value));
    emit EvOffer(msg.value);
  }

  function cancel() external only(seller) inState(State.Created) {
    state = State.Canceled;
    (bool success, ) = seller.call{value: address(this).balance}("");
    require(success, ErrSendFailed("cancel"));
    emit EvCancel(price * 2);
  }

  function purchase() external payable inState(State.Created) {
    require(msg.value == price * 2);
    state = State.Locked;
    buyer = payable(msg.sender);
    emit EvPurchase(msg.value);
  }

  function received() external only(buyer) inState(State.Locked) {
    state = State.Received;
    (bool success, ) = buyer.call{value: price}("");
    require(success, ErrSendFailed("received"));
    emit EvReceive(price);
  }

  function refund() external only(seller) inState(State.Received) {
    state = State.Refunded;
    // (bool success, ) = seller.call{value: price * 3}("");
    // require(success, ErrSendFailed("refund"));
    seller.transfer(price * 3);
    emit EvRefund(price * 3);
  }
}
