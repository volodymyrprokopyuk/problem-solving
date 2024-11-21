// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

// ERC-721 Non-fungible token NFT standard. Each token is unique and represents
// a distinguishable asset. Each token ownership must be tracked individually.
// Each distinguishable asset is represented by a unique, immutable token ID.
// The combination of (address(contract), tokenID) is globally unique on a
// blockchain. The NFT creation (minting) and destroy (burning) is not
// explicitly handled by the interface
interface IERC721 {
  // On the new token creation, except the massive token creation in the
  // constructor, the token destroy, the token ownership change via
  // safeTransferFrom() and transferFrom()
  event Transfer(address indexed from, address indexed to, uint tokenID);
  // On approve() an address is approved for the NFT
  event Approval(address indexed owner, address indexed delegee, uint tokenID);
  // On setApprovalForAll() when an operator is enabled or disabled to manages
  // all tokens of the owner
  event ApprovalForAll(
    address indexed owner, address indexed operator, bool approvad
  );

  function ownerOf(uint tokenID) external view returns (address owner);
  function balanceOf(address owner) external view returns (uint numberOfTokens);
  // The current owner, an approved address for the NFT, or an approved operator
  // for the current owner represented by the msg.sender transfers the token
  // from the current owner to a new owner identified by a non-zero address. If
  // the destination is a smart contract (to.code.length > 0), the reception of
  // the NFT is confirmed by calling the onERC721Received()
  function safeTransferFrom(address from, address to, uint tokenID)
    external payable;
  function safeTransferFrom(
    address from, address to, uint tokenID, bytes memory data
  ) external payable;
  // The msg.sender is responsible to confirm that the destination is capable of
  // receiving NTFs, otherwise, the NTF may be permanently lost
  function transferFrom(address from, address to, uint tokenID)
    external payable;
  // The current owner or one of approved operators changes or reaffirms the
  // single approved address for the NFT
  function approve(address delegee, uint tokenID) external payable;
  // Returns the approved address for the NTF
  function getApproved(uint tokenID) external view returns (address delegee);
  // The current owner enables or disables an operator to manage all tokens of
  // the owner. Multiple operators can manage tokens of an owner
  function setApprovalForAll(address operator, bool approved) external;
  // Checks if the operator is enabled to manage all tokens of the owner.
  // Multiple operators can tokens of an owner
  function isApprovedForAll(address owner, address operator)
    external view returns (bool approved);
}

// The interface must be implemented by a contract that accepts safe transfers
// of NTFs
interface IERC721TokenReceiver {
  // After a safe transfer of an NFT the recipient confirms the reception of the
  // NFT by returning IERC721TokenReceiver.onERC721Received.selector
  function onERC721Received(
    address operator, address from, uint tokenID, bytes memory data
  ) external returns (bytes4);
}

contract ERC721NFT is IERC721 {
  address minter;
  mapping(uint => address) tokenOwner;
  mapping(address => uint) ownerTokens;
  mapping(uint => address) tokenDelegee;
  // owner => operator => approved/revoked
  mapping(address => mapping(address => bool)) operators;

  error ErrUnauthorized(address sender);
  error ErrZeroAddress(address addr);
  error ErrInvalidToken(uint tokenID);
  error ErrOwnedToken(uint tokenID);
  error ErrUnauthorizedTransfer(address account, uint tokenID);
  error ErrDoesNotOwn(address from, uint tokenID);
  error ErrTokenReceive(address from, address to, uint tokenID);

  modifier only(address target) {
    require(msg.sender == target, ErrUnauthorized(msg.sender));
    _;
  }

  modifier newToken(uint tokenID) {
    require(tokenOwner[tokenID] == address(0), ErrOwnedToken(tokenID));
    _;
  }

  modifier validToken(uint tokenID) {
    require(tokenOwner[tokenID] != address(0), ErrInvalidToken(tokenID));
    _;
  }

  modifier nonZeroAddress(address addr) {
    require(addr != address(0), ErrZeroAddress(addr));
    _;
  }

  constructor(address mnt) {
    minter = mnt;
  }

  function mint(address to, uint tokenID)
    external only(minter) newToken(tokenID) nonZeroAddress(to) {
    tokenOwner[tokenID] = to;
    ownerTokens[to]++;
    emit Transfer(address(0), to, tokenID);
  }

  function burn(uint tokenID) external only(minter) validToken(tokenID) {
    delete tokenDelegee[tokenID];
    address owner = tokenOwner[tokenID];
    delete tokenOwner[tokenID];
    ownerTokens[owner]--;
    emit Transfer(owner, address(0), tokenID);
  }

  function isOwner(uint tokenID) internal view returns (bool) {
    return tokenOwner[tokenID] == msg.sender;
  }

  function isDelegee(uint tokenID) internal view returns (bool) {
    return tokenDelegee[tokenID] == msg.sender;
  }

  function isOperator(uint tokenID) internal view returns (bool) {
    address owner = tokenOwner[tokenID];
    return operators[owner][msg.sender];
  }

  modifier ownerOrOperator(uint tokenID) {
    require(
      isOwner(tokenID) || isOperator(tokenID),
      ErrUnauthorizedTransfer(msg.sender, tokenID)
    );
    _;
  }

  modifier ownerOrDelegeeOrOperator(uint tokenID) {
    require(
      isOwner(tokenID) || isDelegee(tokenID) || isOperator(tokenID),
      ErrUnauthorizedTransfer(msg.sender, tokenID)
    );
    _;
  }

  function ownerOf(uint tokenID)
    external view validToken(tokenID) returns (address) {
    return tokenOwner[tokenID];
  }

  function balanceOf(address owner)
    external view nonZeroAddress(owner) returns (uint) {
    return ownerTokens[owner];
  }

  function transferFrom(address from, address to, uint tokenID)
    public payable validToken(tokenID) ownerOrDelegeeOrOperator(tokenID)
    nonZeroAddress(to) {
    require(tokenOwner[tokenID] == from, ErrDoesNotOwn(from, tokenID));
    delete tokenOwner[tokenID];
    tokenOwner[tokenID] = to;
    ownerTokens[from]--;
    ownerTokens[to]++;
  }

  function safeTransferFrom(
    address from, address to, uint tokenID, bytes memory data
  ) public payable {
    transferFrom(from, to, tokenID);
    if (to.code.length > 0) {
      bytes4 received = IERC721TokenReceiver(to).onERC721Received(
        msg.sender, from, tokenID, data
      );
      require(
        received == IERC721TokenReceiver.onERC721Received.selector,
        ErrTokenReceive(from, to, tokenID)
      );
    }
    emit Transfer(from, to, tokenID);
  }

  function safeTransferFrom(address from, address to, uint tokenID)
    external payable {
    safeTransferFrom(from, to, tokenID, "");
  }

  function approve(address delegee, uint tokenID) external payable
    validToken(tokenID) ownerOrOperator(tokenID) nonZeroAddress(delegee) {
    tokenDelegee[tokenID] = delegee;
    emit Approval(msg.sender, delegee, tokenID);
  }

  function getApproved(uint tokenID) external view
    validToken(tokenID) returns (address) {
    return tokenDelegee[tokenID];
  }

  function setApprovalForAll(address operator, bool approved)
    external {
    operators[msg.sender][operator] = approved;
    emit ApprovalForAll(msg.sender, operator, approved);
  }

  function isApprovedForAll(address owner, address operator)
    external view returns (bool) {
    return operators[owner][operator];
  }
}
