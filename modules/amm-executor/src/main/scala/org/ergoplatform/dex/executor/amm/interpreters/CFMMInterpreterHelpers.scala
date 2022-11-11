package org.ergoplatform.dex.executor.amm.interpreters

import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R4}
import org.ergoplatform.dex.configs.MonetaryConfig
import org.ergoplatform.dex.domain.AssetAmount
import org.ergoplatform.dex.domain.amm.CFMMOrder.{Swap, SwapMultiAddress}
import org.ergoplatform.dex.domain.amm.{CFMMOrder, CFMMPool}
import org.ergoplatform.dex.executor.amm.config.ExchangeConfig
import org.ergoplatform.dex.executor.amm.domain.errors.{ExecutionFailed, PriceTooHigh, PriceTooLow}
import org.ergoplatform.ergo.TokenId
import org.ergoplatform.ergo.syntax._
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, ErgoScriptPredef, Pay2SAddress}
import sigmastate.Values.IntConstant
import sigmastate.eval._
import sigmastate.{SInt, Values}
import special.collection.Coll

final class CFMMInterpreterHelpers(
  exchange: ExchangeConfig,
  monetary: MonetaryConfig
)(implicit encoder: ErgoAddressEncoder) {

  val minerFeeProp: Values.ErgoTree = Pay2SAddress(ErgoScriptPredef.feeProposition()).script
  val dexFeeProp: Values.ErgoTree   = exchange.rewardAddress.toErgoTree

  def swapParams(swap: CFMMOrder.SwapAny, pool: CFMMPool): Either[ExecutionFailed, (AssetAmount, AssetAmount, Long)] = {
    val params = swap match {
      case s: Swap             => s.params
      case s: SwapMultiAddress => s.params
    }
    val input  = params.input
    val output = pool.outputAmount(input)
    val dexFee = (BigInt(output.value) * params.dexFeePerTokenNum /
      params.dexFeePerTokenDenom - monetary.minerFee).toLong
    val maxDexFee = swap.box.value - monetary.minerFee - monetary.minBoxValue
    if (output < params.minOutput) Left(PriceTooHigh(swap.poolId, params.minOutput, output))
    else if (dexFee > maxDexFee) Left(PriceTooLow(swap.poolId, maxDexFee, dexFee))
    else Right((input, output, dexFee))
  }

  def mkTokens(tokens: (TokenId, Long)*): Coll[(ErgoBox.TokenId, Long)] =
    Colls.fromItems(tokens.map { case (k, v) => k.toErgo -> v }: _*)

  def mkPoolRegs(pool: CFMMPool): Map[NonMandatoryRegisterId, Values.Constant[SInt.type]] =
    scala.Predef.Map(
      (R4: NonMandatoryRegisterId) -> IntConstant(pool.feeNum)
    )
}
