import { describe, test, expect } from "vitest"
import { ethers } from "hardhat"

describe("Token contract", () => {
  test("Assign total supply of tokens to the owner", async () => {
    const [owner] = await ethers.getSigners()
    const contract = await ethers.deployContract("Token")
    const totalSupply = await contract.totalSupply()
    const ownerBalance = await contract.balanceOf(owner.address)
    expect(ownerBalance).toBe(totalSupply)
  })
})
