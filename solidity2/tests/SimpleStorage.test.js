import { ethers } from "hardhat"

describe("SimpleStorage", () => {
  test("Set and get value", async () => {
    const storage = await ethers.deployContract("SimpleStorage")
    let value = await storage.get()
    expect(value).toBe(0n)
    await storage.set(1n)
    value = await storage.get()
    expect(value).toBe(1n)
  })
})
