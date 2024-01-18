package org.ergoplatform.dex.executor.amm.domain

import cats.syntax.show._
import org.ergoplatform.dex.domain.AssetAmount
import org.ergoplatform.dex.domain.amm.PoolId
import org.ergoplatform.dex.domain.amm.OrderId
import org.ergoplatform.ergo.{BoxId, SErgoTree}
import tofu.Errors

object errors {

  abstract class ExecutionFailed(msg: String) extends Exception(msg)

  object ExecutionFailed extends Errors.Companion[ExecutionFailed]

  final case class PriceTooHigh(poolId: PoolId, minOutput: AssetAmount, actualOutput: AssetAmount)
    extends ExecutionFailed(
      s"Price slipped up too much for Pool{id=$poolId}. {minOutput=${minOutput.show}, actualOutput=${actualOutput.show}}"
    )

  final case class PriceTooLow(poolId: PoolId, maxDexFee: Long, actualDexFee: Long, quote: Long)
    extends ExecutionFailed(
      s"Price slipped down too much for Pool{id=$poolId}. {maxDexFee=${maxDexFee.show}, actualDexFee=${actualDexFee.show}, quote=${quote}"
    )

  final case class IncorrectMultiAddressTree(poolId: PoolId, orderId: BoxId, tree: SErgoTree, err: String)
    extends ExecutionFailed(
      s"Incorrect multi address tree for pool $poolId and order $orderId: $tree. Err is: $err"
    )

  final case class EmptyOutputForDexTokenFee(poolId: PoolId, orderId: BoxId)
    extends ExecutionFailed(
      s"There is no output for make box with spf fee."
    )

  final case class EmptyOutputForSwapDexy(poolId: PoolId)
    extends ExecutionFailed(
      s"There is no output for make dexy swap box."
    )

  final case class EmptyOutputForDepositDexy(poolId: PoolId)
    extends ExecutionFailed(
      s"There is no output for make dexy deposit box."
    )

  final case class NegativeDexFee(poolId: PoolId, orderId: OrderId, dexFee: Long)
    extends ExecutionFailed(
      s"Dex fee is negative ${dexFee} for pool: $poolId, order: $orderId."
    )
}
