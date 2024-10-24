import { ethers } from "hardhat"
import { loadFixture } from "@nomicfoundation/hardhat-toolbox/network-helpers"
import { listenForEvent, toEmitWithArgs } from "./util"

expect.extend({ toEmitWithArgs })

describe("BlindAuction", () => {
  async function contractAndAccounts() {
    const [benef, acc1, acc2] = await ethers.getSigners()
    const BlindAuction = await ethers.getContractFactory("BlindAuction")
    const auction = await BlindAuction.deploy(benef)
    return { auction, benef, acc1, acc2 }
  }
  test("Bid, bid higher, reveal, withdraw, end auction", async () => {
    const { auction, benef, acc1, acc2 } = await loadFixture(contractAndAccounts)
    const abi = ethers.AbiCoder.defaultAbiCoder()
    let value = ethers.keccak256(abi.encode(["uint256"], [1n]))
    await auction.connect(acc1).placeBid(value, { value: 1n })
    value = ethers.keccak256(abi.encode(["uint256"], [2n]))
    await auction.connect(acc1).placeBid(value, { value: 2n })
    value = ethers.keccak256(abi.encode(["uint256"], [3n]))
    await auction.connect(acc2).placeBid(value, { value: 3n })
    await auction.connect(acc1).reveal([1n, 2n])
    const evNewTopBid = listenForEvent(auction, "EvNewTopBid")
    await auction.connect(acc2).reveal([3n])
    expect(await evNewTopBid).toEmitWithArgs(acc2.address, 3n)
    const evWithdraw = listenForEvent(auction, "EvWithdraw")
    await auction.connect(acc1).withdraw()
    expect(await evWithdraw).toEmitWithArgs(acc1.address, 3n)
    const evAuctionEnd = listenForEvent(auction, "EvAuctionEnd")
    await auction.endAuction()
    expect(await evAuctionEnd).toEmitWithArgs(acc2.address, 3n)
  })
})
