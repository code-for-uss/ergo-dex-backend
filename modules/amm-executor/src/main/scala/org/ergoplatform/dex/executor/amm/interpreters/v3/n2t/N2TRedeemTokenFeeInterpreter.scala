package org.ergoplatform.dex.executor.amm.interpreters.v3.n2t

import cats.Monad
import cats.effect.concurrent.Ref
import cats.syntax.either._
import cats.syntax.semigroup._
import org.ergoplatform._
import org.ergoplatform.dex.configs.MonetaryConfig
import org.ergoplatform.dex.domain.amm.CFMMOrder._
import org.ergoplatform.dex.domain.amm.CFMMPool
import org.ergoplatform.dex.domain.{BoxInfo, NetworkContext, ResolverOutput}
import org.ergoplatform.dex.executor.amm.config.ExchangeConfig
import org.ergoplatform.dex.executor.amm.domain.errors.{ExecutionFailed, IncorrectMultiAddressTree}
import org.ergoplatform.dex.executor.amm.interpreters.CFMMInterpreterHelpers
import org.ergoplatform.dex.protocol.ErgoTreeSerializer
import org.ergoplatform.dex.protocol.amm.AMMContracts
import org.ergoplatform.dex.protocol.amm.AMMType.N2T_CFMM
import org.ergoplatform.ergo.BoxId
import org.ergoplatform.ergo.domain.Output
import org.ergoplatform.ergo.state.{Predicted, Traced}
import org.ergoplatform.ergo.syntax._
import org.ergoplatform.wallet.interpreter.ErgoUnsafeProver
import sigmastate.interpreter.ProverResult
import tofu.syntax.monadic._
import tofu.syntax.raise._

final class N2TRedeemTokenFeeInterpreter[F[_]: Monad: ExecutionFailed.Raise](
  exchange: ExchangeConfig,
  execution: MonetaryConfig,
  ref: Ref[F, NetworkContext],
  helpers: CFMMInterpreterHelpers
)(implicit contracts: AMMContracts[N2T_CFMM], e: ErgoAddressEncoder) {
  import helpers._

  def redeem(
    redeem: RedeemTokenFee,
    pool: CFMMPool,
    dexFeeOutput: Output
  ): F[
    (
      ErgoLikeTransaction,
      Traced[Predicted[CFMMPool]],
      Traced[Predicted[ResolverOutput]],
      Option[Traced[Predicted[ResolverOutput]]]
    )
  ] = ref.get.flatMap { ctx =>
    Either
      .catchNonFatal(ErgoTreeSerializer.default.deserialize(redeem.params.redeemer))
      .leftMap(s => IncorrectMultiAddressTree(pool.poolId, redeem.box.boxId, redeem.params.redeemer, s.getMessage))
      .toRaise
      .map { redeemer =>
        val poolBox   = pool.box
        val redeemBox = redeem.box

        val redeemIn = new Input(redeemBox.boxId.toErgo, ProverResult.empty)
        val poolIn   = new Input(poolBox.boxId.toErgo, ProverResult.empty)
        val dexFeeIn = new Input(dexFeeOutput.boxId.toErgo, ProverResult.empty)

        val inLP             = redeem.params.lp
        val (shareX, shareY) = pool.shares(inLP)

        val poolOut = new ErgoBoxCandidate(
          value          = poolBox.value - shareX.value,
          ergoTree       = contracts.pool,
          creationHeight = ctx.currentHeight,
          additionalTokens = mkPoolTokens(
            pool,
            amountLP = pool.lp.value + inLP.value,
            amountY  = pool.y.value - shareY.value
          ),
          additionalRegisters = mkPoolRegs(pool)
        )

        val minerFee    = execution.minerFee min redeem.maxMinerFee
        val minerFeeBox = new ErgoBoxCandidate(minerFee, minerFeeProp, ctx.currentHeight)

        val dexFeeTokensReturn: Seq[(ergo.TokenId, Long)] =
          (Map(exchange.spectrumToken -> redeem.params.dexFee) |+| dexFeeOutput.assets
            .map(asset => asset.tokenId -> asset.amount)
            .toMap).toSeq

        val dexFeeBox = new ErgoBoxCandidate(
          dexFeeOutput.value - minerFeeBox.value,
          P2PKAddress(sk.publicImage).script,
          ctx.currentHeight,
          additionalTokens = mkTokens(dexFeeTokensReturn: _*)
        )

        val returnBox = new ErgoBoxCandidate(
          value            = redeemBox.value + shareX.value,
          ergoTree         = redeemer,
          creationHeight   = ctx.currentHeight,
          additionalTokens = mkTokens(shareY.id -> shareY.value)
        )

        val inputs             = Vector(poolIn, redeemIn, dexFeeIn)
        val outs               = Vector(poolOut, returnBox, dexFeeBox, minerFeeBox)
        val tx                 = ErgoUnsafeProver.prove(UnsignedErgoLikeTransaction(inputs, outs), sk)
        val nextPoolBox        = poolOut.toBox(tx.id, 0)
        val boxInfo            = BoxInfo(BoxId.fromErgo(nextPoolBox.id), nextPoolBox.value)
        val nextPool           = pool.redeem(inLP, boxInfo)
        val predictedDexOutput = Output.predicted(Output.fromErgoBox(tx.outputs(2)), dexFeeOutput.boxId)
        (tx, nextPool, predictedDexOutput, Option.empty)
      }
  }
}
