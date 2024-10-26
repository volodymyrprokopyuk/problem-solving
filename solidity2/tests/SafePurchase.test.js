import { ethers } from "hardhat"
import { loadFixture } from "@nomicfoundation/hardhat-toolbox/network-helpers"
import { listenForEvent, toEmitWithArgs } from "./util"

expect.extend({ toEmitWithArgs })

describe("SafePurchase", () => {
  const price = 10n
  async function contractAndAccounts() {
    const SafePurchase = await ethers.getContractFactory("SafePurchase")
    const purchase = await SafePurchase.deploy({ value: price * 2n })
    const [seller, buyer] = await ethers.getSigners()
    return { purchase, seller, buyer }
  }
  test("Offer, purchase, receive, refund", async () => {
    const { purchase, seller, buyer } = await loadFixture(contractAndAccounts)
    const evPurchase = listenForEvent(purchase, "EvPurchase")
    await purchase.connect(buyer).purchase({ value: price * 2n })
    expect(await evPurchase).toEmitWithArgs(price * 2n)
    await purchase.connect(buyer).received()
    const evRefund = listenForEvent(purchase, "EvRefund")
    await purchase.refund()
    expect(await evRefund).toEmitWithArgs(price * 3n)
  })
})
