// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract PaymentClaim {
  address payable private owner;
  bool private active;
  mapping(uint => bool) nonces;

  event EvPayment(address indexed claimer, uint value);

  error ErrUnauthorized(address account);
  error ErrDeactivated();
  error ErrReusedNonce(uint nonce);
  error ErrInvalidClaim(address claimer, uint value, uint nonce);
  error ErrInsufficientFunds(uint required);
  error ErrSendFailed(string action);

  modifier only(address target) {
    require(msg.sender == target, ErrUnauthorized(msg.sender));
    _;
  }

  modifier isActive() {
    require(active, ErrDeactivated());
    _;
  }

  constructor() payable {
    owner = payable(msg.sender);
    active = true;
  }

  function verifyClaim(uint value, bytes memory claim)
    internal view returns (uint) {
    (uint nonce, uint8 v, bytes32 r, bytes32 s) =
      abi.decode(claim, (uint, uint8, bytes32, bytes32));
    require(!nonces[nonce], ErrReusedNonce(nonce));
    bytes32 hash = keccak256(abi.encode(msg.sender, value, nonce));
    address signer = ecrecover(hash, v, r, s);
    require(signer == owner, ErrInvalidClaim(msg.sender, value, nonce));
    return nonce;
  }

  function claimPayment(uint value, bytes memory claim) external isActive {
    uint nonce = verifyClaim(value, claim);
    require(value <= address(this).balance, ErrInsufficientFunds(value));
    nonces[nonce] = true;
    (bool success, ) = msg.sender.call{value: value}("");
    require(success, ErrSendFailed("claim"));
    emit EvPayment(msg.sender, value);
  }

  function deactivate() external only(owner) isActive {
    active = false;
    (bool success, ) = owner.call{value: address(this).balance}("");
    require(success, ErrSendFailed("refund"));
  }
}
