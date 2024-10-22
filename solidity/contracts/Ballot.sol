// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

// Voting with delegation
contract Ballot {
  struct Voter {
    uint256 weight;
    bool voted;
    address delegate;
    uint256 vote;
  }

  struct Proposal {
    bytes32 name;
    uint256 voteCount;
  }

  address public chairPerson;
  mapping(address => Voter) public voters;
  Proposal[] public proposals;

  constructor(bytes32[] memory proposalNames) {
    chairPerson = msg.sender;
    voters[chairPerson].weight = 1;
    for (uint256 i = 0; i < proposalNames.length; i++) {
      Proposal memory proposal = Proposal({
        name: proposalNames[i], voteCount: 0
      });
      proposals.push(proposal);
    }
  }

  function giveRightToVote(address voter) external {
    require(msg.sender == chairPerson, "only chair person can give right to vote");
    require(!voters[voter].voted, "the voter already voted");
    require(voters[voter].weight == 0, "not voted voter with non-zero weight");
    voters[voter].weight = 1;
  }
}
