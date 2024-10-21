import { ethers } from "hardhat"
import { listenForEvent, toEmitWithArgs } from "./util"

expect.extend({ toEmitWithArgs })

describe("SimpleCoin", () => {
  test("Mint and send", async () => {
    const coin = await ethers.deployContract("SimpleCoin");
    const [minter, acc1] = await ethers.getSigners();
    await coin.mint(acc1, 2n);
    const acc1Balance = await coin.balances(acc1.address);
    expect(acc1Balance).toBe(2n);
    const event = listenForEvent(coin, "Sent")
    await coin.connect(acc1).send(minter, 1n);
    expect(await event).toEmitWithArgs(acc1.address, minter.address, 1n);
  })
})
