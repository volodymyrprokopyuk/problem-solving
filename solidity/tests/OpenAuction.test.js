import { ethers } from "hardhat"
import { loadFixture } from "@nomicfoundation/hardhat-toolbox/network-helpers"
import { listenForEvent, toEmitWithArgs } from "./util"

expect.extend({ toEmitWithArgs })

describe("OpenAuction", () => {
  async function contractAndAccounts() {
    const [benef, acc1, acc2] = await ethers.getSigners()
    const OpenAuction = await ethers.getContractFactory("OpenAuction")
    const auction = await OpenAuction.deploy(60, benef)
    return { auction, benef, acc1, acc2 }
  }
  test("Bid, bid higher, withdraw, end auction", async () => {
    const { auction, benef, acc1, acc2 } = await loadFixture(contractAndAccounts)
    // console.log(await ethers.provider.getBalance(benef.address))
    let newTopBid = listenForEvent(auction, "EvNewTopBid")
    await auction.connect(acc1).bid({ value: 1n })
    expect(await newTopBid).toEmitWithArgs(acc1.address, 1n)
    newTopBid = listenForEvent(auction, "EvNewTopBid")
    await auction.connect(acc2).bid({ value: 2n })
    expect(await newTopBid).toEmitWithArgs(acc2.address, 2n)
    const withdrawn = listenForEvent(auction, "EvWithdraw")
    await auction.connect(acc1).withdraw()
    expect(await withdrawn).toEmitWithArgs(acc1.address, 1n)
    const auctionEnd = listenForEvent(auction, "EvAuctionEnd")
    await auction.endAuction()
    expect(await auctionEnd).toEmitWithArgs(acc2.address, 2n)
    // console.log(await ethers.provider.getBalance(benef.address))
  })
})
