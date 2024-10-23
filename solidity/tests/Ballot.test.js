import { ethers } from "hardhat"
import { loadFixture } from "@nomicfoundation/hardhat-toolbox/network-helpers"

describe("Ballot", () => {
  async function contractAndAccounts() {
    const proposalNames = ["Proposal A", "Proposal B"].map(ethers.encodeBytes32String)
    const Ballot = await ethers.getContractFactory("Ballot")
    const ballot = await Ballot.deploy(proposalNames)
    const [chair, acc1] = await ethers.getSigners()
    return [ballot, chair, acc1]
  }
  test("Vote directly", async () => {
    const [ballot, chair, acc1] = await loadFixture(contractAndAccounts)
    await ballot.giveRightToVote(acc1.address)
    await ballot.connect(acc1).vote(0)
    const winner = await ballot.winningProposal()
    expect(ethers.decodeBytes32String(winner.name)).toBe("Proposal A")
    expect(winner.voteCount).toBe(1n)
  })
  test("Delegate vote before delegee has voted", async () => {
    const [ballot, chair, acc1] = await loadFixture(contractAndAccounts)
    await ballot.giveRightToVote(acc1.address)
    await ballot.connect(acc1).delegate(chair)
    await ballot.vote(1)
    const winner = await ballot.winningProposal()
    expect(ethers.decodeBytes32String(winner.name)).toBe("Proposal B")
    expect(winner.voteCount).toBe(2n)
  })
  test("Delegate vote after delegee has voted", async () => {
    const [ballot, chair, acc1] = await loadFixture(contractAndAccounts)
    await ballot.giveRightToVote(acc1.address)
    await ballot.vote(1)
    await ballot.connect(acc1).delegate(chair)
    const winner = await ballot.winningProposal()
    expect(ethers.decodeBytes32String(winner.name)).toBe("Proposal B")
    expect(winner.voteCount).toBe(2n)
  })
})
