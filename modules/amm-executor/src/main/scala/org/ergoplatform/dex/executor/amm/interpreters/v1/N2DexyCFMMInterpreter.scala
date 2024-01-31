package org.ergoplatform.dex.executor.amm.interpreters.v1

import cats.syntax.either._
import cats.{Functor, Monad}
import org.ergoplatform._
import org.ergoplatform.dex.configs.MonetaryConfig
import org.ergoplatform.dex.domain.amm.CFMMOrder._
import org.ergoplatform.dex.domain.amm._
import org.ergoplatform.dex.domain.{BoxInfo, NetworkContext, ResolverOutput}
import org.ergoplatform.dex.executor.amm.config.ExchangeConfig
import org.ergoplatform.dex.executor.amm.domain.errors._
import org.ergoplatform.dex.executor.amm.interpreters.CFMMInterpreterHelpers
import InterpreterV1.InterpreterTracing
import org.ergoplatform.dex.executor.amm.services.{DepositDexyOutputResolver, DexOutputResolver, SwapDexyOutputResolver}
import org.ergoplatform.dex.protocol.ErgoTreeSerializer
import org.ergoplatform.dex.protocol.amm.AMMContracts
import org.ergoplatform.dex.protocol.amm.AMMType.N2Dexy_CFMM
import org.ergoplatform.ergo.BoxId
import org.ergoplatform.ergo.domain.Output
import org.ergoplatform.ergo.services.explorer.ErgoExplorer
import org.ergoplatform.ergo.state.{Predicted, Traced}
import org.ergoplatform.ergo.syntax._
import sigmastate.eval.Extensions._
import sigmastate.eval._
import org.ergoplatform.wallet.interpreter.ErgoUnsafeProver
import sigmastate.interpreter.ProverResult
import tofu.logging.Logs
import tofu.syntax.embed._
import tofu.syntax.monadic._
import tofu.syntax.raise._

final class N2DexyCFMMInterpreter[F[_]: Monad: ExecutionFailed.Raise](
  exchange: ExchangeConfig,
  execution: MonetaryConfig,
  ctx: NetworkContext
)(implicit
  contracts: AMMContracts[N2Dexy_CFMM],
  encoder: ErgoAddressEncoder,
  dexResolver: DexOutputResolver[F],
  swapDexyResolver: SwapDexyOutputResolver[F],
  depositDexyResolver: DepositDexyOutputResolver[F]
) extends InterpreterV1[N2Dexy_CFMM, F] {

  val helpers = new CFMMInterpreterHelpers(exchange, execution)

  import helpers._

  // TODO: should complete deposit and redeem actions
  def deposit(
    deposit: DepositErgFee,
    pool: CFMMPool
  ): F[
    (
      ErgoLikeTransaction,
      Traced[Predicted[CFMMPool]],
      Traced[Predicted[ResolverOutput]],
      Option[Traced[Predicted[ResolverOutput]]]
    )
  ] =
    dexResolver.getLatest
      .flatMap(_.orRaise[F](EmptyOutputForDexTokenFee(pool.poolId, deposit.box.boxId)))
      .flatMap { dexFeeOutput =>
        depositDexyResolver.getLatest.flatMap(_.orRaise[F](EmptyOutputForDepositDexy(pool.poolId))).map {
          depositDexyOutput =>
            val poolBox0 = pool.box
            val depositBox = deposit.box
            val depositIn = new Input(depositBox.boxId.toErgo, ProverResult.empty)
            val poolIn = new Input(poolBox0.boxId.toErgo, ProverResult.empty)
            val depositDexyIn = new Input(depositDexyOutput.boxId.toErgo, ProverResult.empty)
            val dexFeeIn = new Input(dexFeeOutput.boxId.toErgo, ProverResult.empty)
            val (inX, inY) = (deposit.params.inX, deposit.params.inY)
            val (rewardLP, change) = pool.rewardLP(inX, inY)
            val (changeX, changeY) =
              (change.filter(_.id == inX.id).map(_.value).sum, change.filter(_.id == inY.id).map(_.value).sum)
            val poolBox1 = new ErgoBoxCandidate(
              value = poolBox0.value + inX.value - changeX,
              ergoTree = contracts.pool,
              creationHeight = ctx.currentHeight,
              additionalTokens = mkPoolTokens(
                pool,
                amountLP = pool.lp.value - rewardLP.value,
                amountY = pool.y.value + inY.value - changeY
              )
            )

            val minerFee = execution.minerFee min deposit.maxMinerFee
            val minerFeeBox = new ErgoBoxCandidate(minerFee, minerFeeProp, ctx.currentHeight)

            val dexFee = deposit.params.dexFee - minerFee

            val dexFeeBox = new ErgoBoxCandidate(
              dexFeeOutput.value + dexFee,
              P2PKAddress(sk.publicImage).script,
              ctx.currentHeight,
              additionalTokens = mkTokens(dexFeeOutput.assets.map(asset => asset.tokenId -> asset.amount): _*)
            )

            val returnBox = new ErgoBoxCandidate(
              value = depositBox.value - inX.value - minerFeeBox.value - dexFee + changeX,
              ergoTree = deposit.params.redeemer.toErgoTree,
              creationHeight = ctx.currentHeight,
              additionalTokens =
                if (changeY > 0) mkTokens(rewardLP.id -> rewardLP.value, inY.id -> changeY)
                else mkTokens(rewardLP.id -> rewardLP.value)
            )
            val depositDexyBox = new ErgoBoxCandidate(
              value = depositDexyOutput.value,
              ergoTree = ErgoTreeSerializer.default.deserialize(depositDexyOutput.ergoTree),
              creationHeight = ctx.currentHeight,
              additionalTokens = depositDexyOutput.assets.map(token => (token.tokenId.toErgo, token.amount)).toColl
            )
            val inputs = Vector(poolIn, depositDexyIn, depositIn, dexFeeIn)
            val outs = Vector(poolBox1, depositDexyBox, returnBox, dexFeeBox, minerFeeBox)
            val tx = ErgoUnsafeProver.prove(UnsignedErgoLikeTransaction(inputs, outs), sk)
            val nextPoolBox = poolBox1.toBox(tx.id, 0)
            val boxInfo = BoxInfo(BoxId.fromErgo(nextPoolBox.id), nextPoolBox.value)
            val nextPool = pool.deposit(inX, inY, boxInfo)
            val predictedDexOutput = Output.predicted(Output.fromErgoBox(tx.outputs(3)), dexFeeOutput.boxId)
            val predictedDexyOutput = Output.predicted(Output.fromErgoBox(tx.outputs(1)), depositDexyOutput.boxId)
            (tx, nextPool, predictedDexOutput, Option(predictedDexyOutput))
        }
      }

  def redeem(
    redeem: RedeemErgFee,
    pool: CFMMPool
  ): F[
    (
      ErgoLikeTransaction,
      Traced[Predicted[CFMMPool]],
      Traced[Predicted[ResolverOutput]],
      Option[Traced[Predicted[ResolverOutput]]]
    )
  ] =
    dexResolver.getLatest
      .flatMap(_.orRaise[F](EmptyOutputForDexTokenFee(pool.poolId, redeem.box.boxId)))
      .map { dexFeeOutput =>
        val poolBox0         = pool.box
        val redeemBox        = redeem.box
        val redeemIn         = new Input(redeemBox.boxId.toErgo, ProverResult.empty)
        val poolIn           = new Input(poolBox0.boxId.toErgo, ProverResult.empty)
        val dexFeeIn         = new Input(dexFeeOutput.boxId.toErgo, ProverResult.empty)
        val inLP             = redeem.params.lp
        val (shareX, shareY) = pool.shares(inLP)
        val poolBox1 = new ErgoBoxCandidate(
          value          = poolBox0.value - shareX.value,
          ergoTree       = contracts.pool,
          creationHeight = ctx.currentHeight,
          additionalTokens = mkPoolTokens(
            pool,
            amountLP = pool.lp.value + inLP.value,
            amountY  = pool.y.value - shareY.value
          )
        )
        val minerFee    = execution.minerFee min redeem.maxMinerFee
        val minerFeeBox = new ErgoBoxCandidate(minerFee, minerFeeProp, ctx.currentHeight)
        val dexFee      = redeem.params.dexFee - minerFee

        val dexFeeBox = new ErgoBoxCandidate(
          dexFeeOutput.value + dexFee,
          P2PKAddress(sk.publicImage).script,
          ctx.currentHeight,
          additionalTokens = mkTokens(dexFeeOutput.assets.map(asset => asset.tokenId -> asset.amount): _*)
        )
        val returnBox = new ErgoBoxCandidate(
          value            = redeemBox.value + shareX.value - minerFeeBox.value - dexFee,
          ergoTree         = redeem.params.redeemer.toErgoTree,
          creationHeight   = ctx.currentHeight,
          additionalTokens = mkTokens(shareY.id -> shareY.value)
        )
        val inputs             = Vector(poolIn, redeemIn, dexFeeIn)
        val outs               = Vector(poolBox1, returnBox, dexFeeBox, minerFeeBox)
        val tx                 = ErgoUnsafeProver.prove(UnsignedErgoLikeTransaction(inputs, outs), sk)
        val nextPoolBox        = poolBox1.toBox(tx.id, 0)
        val boxInfo            = BoxInfo(BoxId.fromErgo(nextPoolBox.id), nextPoolBox.value)
        val nextPool           = pool.redeem(inLP, boxInfo)
        val predictedDexOutput = Output.predicted(Output.fromErgoBox(tx.outputs(2)), dexFeeOutput.boxId)
        (tx, nextPool, predictedDexOutput, Option.empty)
      }

  def swap(
    swap: SwapErg,
    pool: CFMMPool
  ): F[
    (
      ErgoLikeTransaction,
      Traced[Predicted[CFMMPool]],
      Traced[Predicted[ResolverOutput]],
      Option[Traced[Predicted[ResolverOutput]]]
    )
  ] =
    dexResolver.getLatest
      .flatMap(_.orRaise[F](EmptyOutputForDexTokenFee(pool.poolId, swap.box.boxId)))
      .flatMap { dexFeeOutput =>
        swapDexyResolver.getLatest.flatMap(_.orRaise[F](EmptyOutputForSwapDexy(pool.poolId))).flatMap {
          swapDexyOutput =>
            swapParamsErgFee(swap, pool).toRaise.flatMap { case (input, output, dexFee) =>
              (swap match {
                case s: SwapP2Pk =>
                  (s.maxMinerFee, s.params.baseAmount, s.params.redeemer.toErgoTree, s.params.minQuoteAmount).pure[F]
                case s: SwapMultiAddress =>
                  Either
                    .catchNonFatal(ErgoTreeSerializer.default.deserialize(s.params.redeemer))
                    .leftMap(err =>
                      IncorrectMultiAddressTree(pool.poolId, s.box.boxId, s.params.redeemer, err.getMessage)
                    )
                    .toRaise
                    .map(tree => (s.maxMinerFee, s.params.baseAmount, tree, s.params.minQuoteAmount))
              }).map { case (maxMinerFee, inputSwap, redeemer, minOutput) =>
                val poolBox0   = pool.box
                val swapBox    = swap.box
                val swapIn     = new Input(swapBox.boxId.toErgo, ProverResult.empty)
                val poolIn     = new Input(poolBox0.boxId.toErgo, ProverResult.empty)
                val swapDexyIn = new Input(swapDexyOutput.boxId.toErgo, ProverResult.empty)
                val dexFeeIn   = new Input(dexFeeOutput.boxId.toErgo, ProverResult.empty)
                val (deltaX, deltaY) =
                  if (input.id == pool.x.id) input.value -> -output.value
                  else -output.value                     -> input.value
                val poolBox1 = new ErgoBoxCandidate(
                  value          = poolBox0.value + deltaX,
                  ergoTree       = contracts.pool,
                  creationHeight = ctx.currentHeight,
                  additionalTokens = mkPoolTokens(
                    pool,
                    amountLP = pool.lp.value,
                    amountY  = pool.y.value + deltaY
                  )
                )
                val minerFee    = execution.minerFee min maxMinerFee
                val minerFeeBox = new ErgoBoxCandidate(minerFee, minerFeeProp, ctx.currentHeight)
                val dexFeeBox = new ErgoBoxCandidate(
                  dexFeeOutput.value + dexFee,
                  P2PKAddress(sk.publicImage).script,
                  ctx.currentHeight,
                  additionalTokens = mkTokens(dexFeeOutput.assets.map(asset => asset.tokenId -> asset.amount): _*)
                )
                val rewardBox =
                  if (inputSwap.isNative)
                    new ErgoBoxCandidate(
                      value            = swapBox.value - input.value - minerFeeBox.value - dexFee,
                      ergoTree         = redeemer,
                      creationHeight   = ctx.currentHeight,
                      additionalTokens = mkTokens(minOutput.id -> output.value)
                    )
                  else
                    new ErgoBoxCandidate(
                      value          = swapBox.value + output.value - minerFeeBox.value - dexFee,
                      ergoTree       = redeemer,
                      creationHeight = ctx.currentHeight
                    )
                val swapDexyBox = new ErgoBoxCandidate(
                  value            = swapDexyOutput.value,
                  ergoTree         = ErgoTreeSerializer.default.deserialize(swapDexyOutput.ergoTree),
                  creationHeight   = ctx.currentHeight,
                  additionalTokens = swapDexyOutput.assets.map(token => (token.tokenId.toErgo, token.amount)).toColl
                )
                val inputs              = Vector(poolIn, swapDexyIn, swapIn, dexFeeIn)
                val outs                = Vector(poolBox1, swapDexyBox, rewardBox, dexFeeBox, minerFeeBox)
                val tx                  = ErgoUnsafeProver.prove(UnsignedErgoLikeTransaction(inputs, outs), sk)
                val nextPoolBox         = poolBox1.toBox(tx.id, 0)
                val boxInfo             = BoxInfo(BoxId.fromErgo(nextPoolBox.id), nextPoolBox.value)
                val nextPool            = pool.swap(input, boxInfo)
                val predictedDexOutput  = Output.predicted(Output.fromErgoBox(tx.outputs(3)), dexFeeOutput.boxId)
                val predictedDexyOutput = Output.predicted(Output.fromErgoBox(tx.outputs(1)), swapDexyOutput.boxId)
                // TODO: complete setPredicted for dexy boxes!
                (tx, nextPool, predictedDexOutput, Some(predictedDexyOutput))
              }
            }
        }
      }

  private def mkPoolTokens(pool: CFMMPool, amountLP: Long, amountY: Long) =
    mkTokens(
      pool.poolId.value -> 1L,
      pool.lp.id        -> amountLP,
      pool.y.id         -> amountY
    )
}

object N2DexyCFMMInterpreter {

  def make[I[_]: Functor, F[_]: Monad: ExecutionFailed.Raise: ExchangeConfig.Has: MonetaryConfig.Has](implicit
                                                                                                      network: ErgoExplorer[F],
                                                                                                      contracts: AMMContracts[N2Dexy_CFMM],
                                                                                                      encoder: ErgoAddressEncoder,
                                                                                                      dexResolver: DexOutputResolver[F],
                                                                                                      swapDexyResolver: SwapDexyOutputResolver[F],
                                                                                                      depositDexyResolver: DepositDexyOutputResolver[F],
                                                                                                      logs: Logs[I, F]
  ): I[InterpreterV1[N2Dexy_CFMM, F]] =
    logs.forService[InterpreterV1[N2Dexy_CFMM, F]].map { implicit l =>
      (
        for {
          exchange   <- ExchangeConfig.access
          execution  <- MonetaryConfig.access
          networkCtx <- NetworkContext.make
        } yield new InterpreterTracing[N2Dexy_CFMM, F]("N2Dexy_CFMM") attach new N2DexyCFMMInterpreter(
          exchange,
          execution,
          networkCtx
        )
      ).embed
    }
}
