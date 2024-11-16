// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract Ballot {
  struct Candidate {
    string name;
    uint votes;
  }
  struct Voter {
    uint weight;
    uint candidate;
    address delegee;
    bool voted;
  }

  address public owner;
  mapping(address => Voter) public voters;
  Candidate[] public candidates;

  error ErrUnauthorized(address account);
  error ErrAlreadyVoted(address voter);
  error ErrAlreadyVoter(address voter);
  error ErrNotEntitled(address account);
  error ErrSelfDelegation(address delegee);
  error ErrDelegationCycle(address delegee);

  modifier only(address target) {
    require(msg.sender == target, ErrUnauthorized(msg.sender));
    _;
  }

  constructor(string[] memory candNames) {
    owner = msg.sender;
    for (uint i = 0; i < candNames.length; i++) {
      candidates.push(Candidate({name: candNames[i], votes: 0}));
    }
  }

  function entitle(address voter) external only(owner) {
    require(!voters[voter].voted, ErrAlreadyVoted(voter));
    require(voters[voter].weight == 0, ErrAlreadyVoter(voter));
    voters[voter].weight = 1;
  }

  function vote(uint candidate) external {
    Voter storage voter = voters[msg.sender];
    require(voter.weight > 0, ErrNotEntitled(msg.sender));
    require(!voter.voted, ErrAlreadyVoted(msg.sender));
    voter.voted = true;
    voter.candidate = candidate;
    candidates[candidate].votes += voter.weight;
  }

  function delegate(address delegee) external {
    require(msg.sender != delegee, ErrSelfDelegation(msg.sender));
    Voter storage voter = voters[msg.sender];
    require(voter.weight > 0, ErrNotEntitled(msg.sender));
    require(!voter.voted, ErrAlreadyVoted(msg.sender));
    while (voters[delegee].delegee != address(0)) {
      delegee = voters[delegee].delegee;
      require(msg.sender != delegee, ErrDelegationCycle(msg.sender));
    }
    Voter storage ultDelegee = voters[delegee];
    voter.voted = true;
    voter.delegee = delegee;
    if (ultDelegee.voted) {
      candidates[ultDelegee.candidate].votes += voter.weight;
    } else {
      ultDelegee.weight += voter.weight;
    }
  }

  function winner() public view returns (Candidate memory) {
    Candidate memory aWinner = candidates[0];
    for (uint i = 0; i < candidates.length; i++) {
      if (candidates[i].votes > aWinner.votes) {
        aWinner = candidates[i];
      }
    }
    return aWinner;
  }
}
