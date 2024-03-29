import { describe, test, expect } from "vitest"
import { ethers } from "hardhat"
import { loadFixture } from "@nomicfoundation/hardhat-toolbox/network-helpers"
const vt = true

describe("Token contract", async () => {
  // Runs once, takes a network snapshot, resets the network on every test
  async function deployContract() {
    const [owner, acc1, acc2] = await ethers.getSigners()
    const contract = await ethers.deployContract("Token")
    return [contract, owner, acc1, acc2]
  }

  test("Assign total supply of tokens to the owner", async () => {
    // Resets the network to the inital state (network snapshot)
    const [contract, owner] = await loadFixture(deployContract)
    const totalSupply = await contract.totalSupply()
    const ownerBalance = await contract.balanceOf(owner.address)
    expect(ownerBalance).toBe(totalSupply)
  })

  test("Transfer tokens between sccounts", async () => {
    const [contract, owner, acc1, acc2] = await loadFixture(deployContract)
    await expect(contract.transfer(acc1.address, 50))
      .changeTokenBalances(contract, [owner, acc1], [-50, 50])
    await expect(contract.connect(acc1).transfer(acc2.address, 20))
      .changeTokenBalances(contract, [acc1, acc2], [-20, 20])
  })

  test("Emit Transfer events", async () => {
    const [contract, owner, acc1] = await loadFixture(deployContract)
    await expect(contract.transfer(acc1.address, 50))
      .emit(contract, "Transfer").withArgs(owner.address, acc1.address, 50)
  })

  test("Revert with insufficient balance", async () => {
    const [contract, owner, acc1, acc2] = await loadFixture(deployContract)
    await expect(contract.connect(acc1).transfer(acc2.address, 20))
      .revertedWith("Insufficient balance")
  })
})
