// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {Ballot} from "contract/Ballot.sol";

contract BallotTest is Test {
  Ballot ballot;
  address vt1;
  address vt2;
  address vt3;
  string[] candidates = ["A", "B", "C"];

  function setUp() public {
    ballot = new Ballot(candidates);
    (vt1, vt2, vt3) = (makeAddr("vt1"), makeAddr("vt2"), makeAddr("vt3"));
    ballot.entitle(vt1); ballot.entitle(vt2); ballot.entitle(vt3);
  }

  function testVote() public {
    vm.prank(vt1);
    ballot.vote(2);
    vm.prank(vt2);
    ballot.vote(0);
    vm.prank(vt3);
    ballot.vote(2);
    Ballot.Candidate memory winner = ballot.winner();
    assertEq(winner.name, "C");
    assertEq(winner.votes, 2);
  }

  function testDelegateVote() public {
    vm.prank(vt1);
    ballot.delegate(vt2);
    vm.prank(vt2);
    ballot.delegate(vt3);
    vm.prank(vt3);
    ballot.vote(2);
    Ballot.Candidate memory winner = ballot.winner();
    assertEq(winner.name, "C");
    assertEq(winner.votes, 3);
  }

  function testVoteDelegate() public {
    vm.prank(vt3);
    ballot.vote(2);
    vm.prank(vt2);
    ballot.delegate(vt3);
    vm.prank(vt1);
    ballot.delegate(vt2);
    Ballot.Candidate memory winner = ballot.winner();
    assertEq(winner.name, "C");
    assertEq(winner.votes, 3);
  }
}
