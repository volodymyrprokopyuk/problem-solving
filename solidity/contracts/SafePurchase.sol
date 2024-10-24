// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract SafePurchase {
  enum State { Created, Canceled, Locked, Received, Refunded }

  address payable public seller;
  address payable public buyer;
  uint256 public price;
  State public state;

  event EvOffer(uint256 price);
  event EvCancel();
  event EvPurchase(uint256 pricex2);
  event EvReceive();
  event EvRefund(uint256 pricex3);

  error ErrNotEvenValue();
  error ErrOnlySeller();
  error ErrOnlyBuyer();
  error ErrInvalidState(State current, State wanted);

  modifier onlySeller() {
    require(msg.sender == seller, ErrOnlySeller());
    _;
  }

  modifier onlyBuyer() {
    require(msg.sender == buyer, ErrOnlyBuyer());
    _;
  }

  modifier inState(State wanted) {
    require(state == wanted, ErrInvalidState(state, wanted));
    _;
  }

  constructor() payable {
    seller = payable(msg.sender);
    price = msg.value / 2;
    if ((price * 2) != msg.value) {
      revert ErrNotEvenValue();
    }
    emit EvOffer(price);
  }

  function cancel() external onlySeller inState(State.Created) {
    state = State.Canceled;
    seller.transfer(address(this).balance);
    emit EvCancel();
  }

  function purchase() external payable inState(State.Created) {
    require(msg.value == (2 * price));
    state = State.Locked;
    buyer = payable(msg.sender);
    emit EvPurchase(msg.value);
  }

  function received() external onlyBuyer inState(State.Locked) {
    state = State.Received;
    buyer.transfer(price);
    emit EvReceive();
  }

  function refund() external onlySeller inState(State.Received) {
    state = State.Refunded;
    seller.transfer(3 * price);
    emit EvRefund(3 * price);
  }
}
