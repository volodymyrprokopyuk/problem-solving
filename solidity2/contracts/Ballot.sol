// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

// Voting with delegation
contract Ballot {
  struct Voter {
    uint256 weight;
    uint256 vote;
    bool voted;
    address delegee;
  }

  struct Proposal {
    bytes32 name;
    uint256 voteCount;
  }

  address public chairperson;
  mapping(address => Voter) public voters;
  Proposal[] public proposals;

  constructor(bytes32[] memory proposalNames) {
    chairperson = msg.sender;
    voters[chairperson].weight = 1;
    for (uint256 i = 0; i < proposalNames.length; i++) {
      Proposal memory proposal = Proposal({
        name: proposalNames[i], voteCount: 0
      });
      proposals.push(proposal);
    }
  }

  function giveRightToVote(address voter) external {
    require(msg.sender == chairperson, "only chairperson can give right to vote");
    require(!voters[voter].voted, "the voter already voted");
    require(voters[voter].weight == 0, "not voted voter with non-zero weight");
    voters[voter].weight = 1;
  }

  function delegate(address to) external {
    Voter storage sender = voters[msg.sender];
    require(msg.sender != to, "self-delegation is disallowed");
    require(sender.weight != 0, "you have no right to vote");
    require(!sender.voted, "you already voted");
    while (voters[to].delegee != address(0)) {
      to = voters[to].delegee;
      require(msg.sender != to, "delegation cycle found");
    }
    Voter storage delegee = voters[to];
    require(delegee.weight >= 1, "delegee has no right to vote");
    sender.voted = true;
    sender.delegee = to;
    if (delegee.voted) {
      proposals[delegee.vote].voteCount += sender.weight;
    } else {
      delegee.weight += sender.weight;
    }
  }

  function vote(uint256 proposal) external {
    Voter storage sender = voters[msg.sender];
    require(sender.weight != 0, "you have no right to vote");
    require(!sender.voted, "you already voted");
    sender.voted = true;
    sender.vote = proposal;
    proposals[proposal].voteCount += sender.weight;
  }

  function winningProposal() public view returns (Proposal memory) {
    Proposal memory winner = proposals[0];
    for (uint256 i = 1; i < proposals.length; i++) {
      if (proposals[i].voteCount > winner.voteCount) {
        winner = proposals[i];
      }
    }
    return winner;
  }
}
