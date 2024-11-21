// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {IERC721, ERC721NFT, IERC721TokenReceiver} from "contract/ERC721NFT.sol";


contract NFTReceiver is IERC721TokenReceiver {
  function onERC721Received(address, address, uint, bytes memory)
    external pure returns (bytes4) {
    return IERC721TokenReceiver.onERC721Received.selector;
  }
}

contract ERC721NFTTest is Test {
  address minter;
  uint tok1 = 1;
  ERC721NFT nft;
  address owner1;
  address owner2;
  address delegee;
  address operator;
  NFTReceiver nftReceiver;

  function setUp() public {
    minter = makeAddr("minter");
    nft = new ERC721NFT(minter);
    (owner1, owner2) = (makeAddr("owner1"), makeAddr("owner2"));
    (delegee, operator) = (makeAddr("delegee"), makeAddr("operator"));
    // Mint a NFT
    vm.prank(minter);
    vm.expectEmit(true, true, false, true);
    emit IERC721.Transfer(address(0), owner1, tok1);
    nft.mint(owner1, tok1);
    assertEq(nft.ownerOf(tok1), owner1);
    assertEq(nft.balanceOf(owner1), 1);
    nftReceiver = new NFTReceiver();
  }

  function testOwnerTransfer() public {
    // Owner transfers
    vm.expectEmit(true, true, false, true);
    emit IERC721.Transfer(owner1, owner2, tok1);
    vm.prank(owner1);
    nft.safeTransferFrom(owner1, owner2, tok1);
    assertEq(nft.ownerOf(tok1), owner2);
    assertEq(nft.balanceOf(owner2), 1);
    assertEq(nft.balanceOf(owner1), 0);
    // Not an owner transfers => ErrUnauthorizedTransfer
    bytes memory err = abi.encodeWithSelector(
      ERC721NFT.ErrUnauthorizedTransfer.selector, owner1, tok1
    );
    vm.expectRevert(err);
    vm.prank(owner1);
    nft.safeTransferFrom(owner1, owner2, tok1);
  }

  function testDelegeeTransfer() public {
    // Approve a delegee for a specific token
    vm.expectEmit(true, true, false, true);
    emit IERC721.Approval(owner1, delegee, tok1);
    vm.prank(owner1);
    nft.approve(delegee, tok1);
    assertEq(nft.getApproved(tok1), delegee);
    // Delegee transfers a NTF
    vm.expectEmit(true, true, false, true);
    emit IERC721.Transfer(owner1, owner2, tok1);
    vm.prank(delegee);
    nft.safeTransferFrom(owner1, owner2, tok1);
    assertEq(nft.ownerOf(tok1), owner2);
    assertEq(nft.balanceOf(owner2), 1);
    assertEq(nft.balanceOf(owner1), 0);
  }

  function testOperatorTransfer() public {
    // Approve an operator for all tokens
    vm.expectEmit(true, true, false, true);
    emit IERC721.ApprovalForAll(owner1, operator, true);
    vm.prank(owner1);
    nft.setApprovalForAll(operator, true);
    assertTrue(nft.isApprovedForAll(owner1, operator));
    // Operators transfers a NFT
    vm.expectEmit(true, true, false, true);
    emit IERC721.Transfer(owner1, owner2, tok1);
    vm.prank(operator);
    nft.safeTransferFrom(owner1, owner2, tok1);
    assertEq(nft.ownerOf(tok1), owner2);
    assertEq(nft.balanceOf(owner2), 1);
    assertEq(nft.balanceOf(owner1), 0);
  }

  function testOwnerTransferToContract() public {
    // Owner transfers to a contract
    address receiver = address(nftReceiver);
    vm.expectEmit(true, true, false, true);
    emit IERC721.Transfer(owner1, receiver, tok1);
    vm.prank(owner1);
    nft.safeTransferFrom(owner1, receiver, tok1);
    assertEq(nft.ownerOf(tok1), receiver);
    assertEq(nft.balanceOf(receiver), 1);
    assertEq(nft.balanceOf(owner1), 0);
  }
}
