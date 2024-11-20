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
  event Approval(address indexed owner, address indexed approvee, uint tokenID);
  // On setApprovalForAll() when an operator is enabled or disabled to manages
  // all tokens of the owner
  event ApprovalForAll(
    address indexed owner, address indexed operator, bool approvad
  );

  function balanceOf(address owner) external view returns (uint numberOfTokens);
  function ownerOf(uint tokenID) external view returns (address owner);
  // The current owner, an approved address for the NFT, or an approved operator
  // for the current owner represented by the msg.sender transfers the token
  // from the current owner to a new owner identified by a non-zero address. If
  // the destination is a smart contract (code size > 0), the reception of the
  // NFT is confirmed by calling the onERC721Received()
  function safeTransferFrom(
    address from, address to, uint tokenID, bytes data
  ) external payable;
  function safeTransferFrom(address from, address to, uint tokenID)
    external payable;
  // The msg.sender is responsible to confirm that the destination is capable of
  // receiving NTFs, otherwise, the NTF may be permanently lost
  function transferFrom(address from, address to, uint tokenID)
    external payable;
  // The current owner or one of approved operators changes or reaffirms the
  // approved address for the NFT
  function approve(address aprovee, uint tokenID) external payable;
  // The current owner enables or disables an operator to manage all tokens of
  // the owner. Multiple operators can manage tokens of an owner
  function setApprovalForAll(address operator, bool approved) external;
  // Returns the approved address for the NTF
  function getApproved(uint tokenID) external view returns (address approvee);
  // Checks if the operator is enabled to manage all tokens of the owner.
  // Multiple operators can tokens of an owner
  function isApprovedForAll(address owner, address operator)
    external view returns (bool approved);
}

// The interface must be implemented by a contract that accepts safe transfers
// of NTFs
interface IERC721TokenReceiver {
  // After a safe transfer of an NFT the recipient confirms the reception of the
  // NFT by returning
  // bytes4(keccak256("onERC721Received(address,address,uint256,bytes)"))
  function onERC721Received(
    address operator, address from, uint tokenID, bytes data
  ) external returns (bytes4);
}
