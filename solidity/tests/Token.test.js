import { ethers } from "hardhat"
import { loadFixture } from "@nomicfoundation/hardhat-toolbox/network-helpers"
import { listenForEvent, toEmitWithArgs } from "./util"

expect.extend({ toEmitWithArgs })

describe("Token", () => {
  async function contractAndAccounts() {
    const token = await ethers.deployContract("Token")
    const [owner, acc1, acc2] = await ethers.getSigners()
    await token.waitForDeployment()
    return [token, owner, acc1, acc2]
  }
  test("Assign total supply to owner", async () => {
    const [token, owner] = await loadFixture(contractAndAccounts)
    const tokenOwner = await token.owner()
    expect(tokenOwner).toBe(owner.address)
    const totalSupply = await token.totalSupply()
    const ownerBalance = await token.balance(owner.address)
    expect(ownerBalance).toBe(totalSupply)
  })
  test("Transfer tokens between accounts", async () => {
    const [token, _, acc1, acc2] = await loadFixture(contractAndAccounts)
    await token.transfer(acc1, 2n)
    const acc1Balance = await token.balance(acc1.address)
    expect(acc1Balance).toBe(2n)
    await token.connect(acc1).transfer(acc2, 1n)
    const acc2Balance = await token.balance(acc2.address)
    expect(acc2Balance).toBe(1n)
  })
  test("Emit Transfer event", async () => {
    const [token, owner, acc1] = await loadFixture(contractAndAccounts)
    const value = 2n
    const event = listenForEvent(token, "Transfer")
    await token.transfer(acc1, value)
    expect(await event).toEmitWithArgs(owner.address, acc1.address, value)
  })
})
