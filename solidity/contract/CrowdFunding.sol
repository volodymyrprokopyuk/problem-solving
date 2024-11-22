// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

contract CrowdFunding {
  struct Campaign {
    address payable owner;
    uint goal;
    uint pledgedTotal;
    bool ended;
    bool canceled;
    bool claimed;
  }

  mapping(uint => address) campaignOwner;
  mapping(uint => Campaign) public campaigns;
  // campaign ID => pledger => pledge value
  mapping(uint => mapping(address => uint)) public pledgedValue;

  event EvLaunch(uint indexed campaignID, address indexed owner, uint goal);
  event EvCancel(uint indexed campaignID);
  event EvPledge(uint indexed campaignID, address indexed pledger, uint value);
  event EvUnpledge(uint indexed campaignID, address indexed pledger, uint value);
  event EvEnd(uint indexed campaignID, bool goalReached);
  event EvClaim(uint indexed campaignID, address indexed owner, uint pledgedTotal);
  event EvWithdraw(uint indexed campaignID, address indexed pledger, uint value);

  error ErrUnauthorized(address sender);
  error ErrInvalidCampaign(uint campaignID);
  error ErrEndedCampaign(bool ended, uint campaignID);
  error ErrGoalReached(bool reached, uint campaignID);
  error ErrClaimedCampaign(bool claimed, uint campaignID);
  error ErrNothingToUnpledge(uint campaingID, address sender);
  error ErrUnpledge(uint campaignID, address pledger);
  error ErrWithdraw(uint campaignID, address pledger);
  error ErrClaim(uint campaignID, address owner);

  modifier onlyOwner(uint campaignID) {
    require(campaignOwner[campaignID] == msg.sender, ErrUnauthorized(msg.sender));
    _;
  }

  modifier validCampaign(uint campaignID) {
    Campaign memory cpg = campaigns[campaignID];
    require(cpg.owner != address(0), ErrInvalidCampaign(campaignID));
    _;
  }

  modifier campaignEnded(bool ended, uint campaignID) {
    Campaign memory cpg = campaigns[campaignID];
    if (ended && !cpg.ended || !ended && cpg.ended) {
      revert ErrEndedCampaign(cpg.ended, campaignID);
    }
    _;
  }

  modifier goalReached(bool reached, uint campaignID) {
    Campaign memory cpg = campaigns[campaignID];
    bool glReached = cpg.pledgedTotal >= cpg.goal;
    if (reached && !glReached || !reached && glReached) {
      revert ErrGoalReached(glReached, campaignID);
    }
    _;
  }

  modifier pledgeClaimed(bool claimed, uint campaignID) {
    Campaign memory cpg = campaigns[campaignID];
    if (claimed && !cpg.claimed || !claimed && cpg.claimed) {
      revert ErrClaimedCampaign(cpg.claimed, campaignID);
    }
    _;
  }

  function launch(uint goal) external returns (uint) {
    uint campaignID = uint(keccak256(abi.encode(block.timestamp)));
    address owner = msg.sender;
    campaigns[campaignID] = Campaign({
      owner: payable(owner), goal: goal, pledgedTotal: 0,
      ended: false, canceled: false, claimed: false
    });
    campaignOwner[campaignID] = owner;
    emit EvLaunch(campaignID, owner, goal);
    return campaignID;
  }

  function cancel(uint campaignID) external
    onlyOwner(campaignID) validCampaign(campaignID)
    campaignEnded(false, campaignID) {
    Campaign storage cpg = campaigns[campaignID];
    cpg.canceled = true;
    cpg.ended = true;
    emit EvCancel(campaignID);
  }

  function pledge(uint campaignID) external payable
    validCampaign(campaignID) campaignEnded(false, campaignID) {
    pledgedValue[campaignID][msg.sender] += msg.value;
    Campaign storage cpg = campaigns[campaignID];
    cpg.pledgedTotal += msg.value;
    emit EvPledge(campaignID, msg.sender, msg.value);
  }

  function unpledge(uint campaignID) external
    validCampaign(campaignID) campaignEnded(false, campaignID) {
    address payable pledger = payable(msg.sender);
    uint value = pledgedValue[campaignID][pledger];
    require(value >= 0, ErrNothingToUnpledge(campaignID, pledger));
    Campaign storage cpg = campaigns[campaignID];
    cpg.pledgedTotal -= value;
    (bool success, ) = pledger.call{value: value}("");
    require(success, ErrUnpledge(campaignID, pledger));
    emit EvUnpledge(campaignID, msg.sender, value);
  }

  function end(uint campaignID) external
    onlyOwner(campaignID) validCampaign(campaignID)
    campaignEnded(false, campaignID) {
    Campaign storage cpg = campaigns[campaignID];
    cpg.ended = true;
    emit EvEnd(campaignID, cpg.pledgedTotal >= cpg.goal);
  }

  function claim(uint campaignID) external
    onlyOwner(campaignID) validCampaign(campaignID)
    campaignEnded(true, campaignID) goalReached(true, campaignID)
    pledgeClaimed(false, campaignID) {
    Campaign storage cpg = campaigns[campaignID];
    cpg.claimed = true;
    address owner = msg.sender;
    (bool success, ) = owner.call{value: address(this).balance}("");
    require(success, ErrClaim(campaignID, owner));
    emit EvClaim(campaignID, owner, cpg.pledgedTotal);
  }

  function withdraw(uint campaignID) external
    validCampaign(campaignID) campaignEnded(true, campaignID)
    goalReached(false, campaignID) {
    address pledger = msg.sender;
    uint value = pledgedValue[campaignID][pledger];
    require(value >= 0, ErrNothingToUnpledge(campaignID, pledger));
    (bool success, ) = pledger.call{value: value}("");
    require(success, ErrWithdraw(campaignID, pledger));
    emit EvWithdraw(campaignID, pledger, value);
  }
}
