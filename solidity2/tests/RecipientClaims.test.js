import { ethers } from "hardhat"
import { loadFixture } from "@nomicfoundation/hardhat-toolbox/network-helpers"
import { listenForEvent, toEmitWithArgs } from "./util"

expect.extend({ toEmitWithArgs })

describe("RecipientClaims", () => {
  async function contractAndAccounts() {
    const claim = await ethers.deployContract("RecipientClaims")
    const [owner, acc1] = await ethers.getSigners()
    return { claim, owner, acc1 }
  }
  test("Sign payment, send off-chain signature, claimPayment", async () => {
    const { claim, owner, acc1 } = await loadFixture(contractAndAccounts)
    const value = 1n, nonce = 1n
    const claimAddress = await claim.getAddress()

    const abi = ethers.AbiCoder.defaultAbiCoder()
    const hash = ethers.keccak256(abi.encode(
      ["address", "uint256", "uint256", "address"],
      [acc1.address, value, nonce, claimAddress]
    ))

    // const hash = ethers.solidityPackedKeccak256(
    //   ["address", "uint256", "uint256", "address"],
    //   [acc1.address, value, nonce, claimAddress]
    // )

    const sig = await owner.signMessage(hash)
    const evPayment = listenForEvent(claim, "EvPayment")
    await claim.connect(acc1).claimPayment(value, nonce, sig)
    exepct(await evPayment).toEmitWithArgs(acc1.address, value)
  })
})
