import { describe, test, expect } from "bun:test"
import { ethers } from "hardhat"

describe("Token", () => {
  test("Assign total supply to owner", async () => {
    const token = await ethers.deployContract("Token")
    const totalSupply = await token.totalSupply()
    const [owner] = await ethers.getSigners()
    const ownerBalance = await token.balance(owner.address)
    expect(ownerBalance).toEqual(totalSupply)
  })
})
