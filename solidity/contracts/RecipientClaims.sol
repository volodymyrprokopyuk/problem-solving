// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract Owner {
  address payable owner;

  constructor() {
    owner = payable(msg.sender);
  }
}

contract Deactivator is Owner {
  bool private inactive;

  error ErrOnlyOwner();
  error ErrAlreadyDeactivated();

  modifier onlyOwner() {
    require(msg.sender == owner, ErrOnlyOwner());
    _;
  }
  modifier isActive() {
    require(!inactive, ErrAlreadyDeactivated());
    _;
  }

  function deactivate() internal onlyOwner isActive {
    inactive = true;
  }
}

contract RecipientClaims is Deactivator {
  mapping(uint256 => bool) nonces;

  error ErrAlreadyClaimed();
  error ErrInvalidSignature();

  function recoverSigner(bytes32 hash, bytes memory sig)
    internal pure returns (address) {
    require(sig.length == 65, ErrInvalidSignature());
    bytes32 r; bytes32 s; uint8 v;
    assembly {
      r := mload(add(sig, 32))
      s := mload(add(sig, 64))
      v := byte(0, mload(add(sig, 96)))
    }
    return ecrecover(hash, v, r, s);
  }

  function claimPayment(uint256 value, uint256 nonce, bytes memory sig)
    external isActive {
    require(!nonces[nonce], ErrAlreadyClaimed());
    nonces[nonce] = true;
    bytes32 hash = keccak256(abi.encodePacked(
      msg.sender, value, nonce, address(this)
    ));
    address signer = recoverSigner(hash, sig);
    require(signer == owner);
    payable(msg.sender).transfer(value);
  }

  function close() external onlyOwner isActive {
    deactivate();
    owner.transfer(address(this).balance);
  }
}
