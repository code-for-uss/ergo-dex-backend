package org.ergoplatform.dex.executor.amm.services

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import derevo.derive
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.ergo.domain.Output
import org.ergoplatform.ergo.services.explorer.ErgoExplorer
import org.ergoplatform.ergo.TokenId
import tofu.higherKind.Mid
import tofu.higherKind.derived.representableK
import tofu.lift.IsoK
import tofu.logging.{Logging, Logs}
import tofu.syntax.logging._
import tofu.syntax.monadic._

@derive(representableK)
trait DepositDexyOutputResolver[F[_]] {
  def getLatest: F[Option[Output]]

  def setPredicted(output: Output): F[Unit]

  def invalidateAndUpdate: F[Unit]
}

object DepositDexyOutputResolver {

  def make[I[_]: Sync, F[_]: Sync](
                                    tokenId: TokenId
                                  )(implicit
                                    explorer: ErgoExplorer[F],
                                    e: ErgoAddressEncoder,
                                    logs: Logs[I, F],
                                    iso: IsoK[F, I]
                                  ): I[DepositDexyOutputResolver[F]] =
    logs.forService[DepositDexyOutputResolver[F]].flatMap { implicit __ =>
      Ref.in[I, F, Option[Output]](null).flatMap { ref =>
        iso
          .to(
            explorer
              .getUtxoByToken(tokenId, 0, 1)
              .flatMap { outs =>
                ref.set(outs.map(Output.fromExplorer).headOption)
              }
          )
          .map { _ =>
            new Tracing[F] attach new InMemory[F](tokenId, ref): DepositDexyOutputResolver[F]
          }
      }
    }

  final private class InMemory[F[_]: Monad](tokenId: TokenId, cache: Ref[F, Option[Output]])(implicit
                                                                                             explorer: ErgoExplorer[F]
  ) extends DepositDexyOutputResolver[F] {
    def getLatest: F[Option[Output]] = cache.get

    def invalidateAndUpdate: F[Unit] =
      explorer.getUtxoByToken(tokenId, 0, 1).flatMap { outputs =>
        outputs.headOption.fold(unit)(output => cache.set(Some(Output.fromExplorer(output))))
      }

    def setPredicted(output: Output): F[Unit] =
      cache.set(Some(output))
  }

  final private class Tracing[F[_]: Monad: Logging] extends DepositDexyOutputResolver[Mid[F, *]] {

    def getLatest: Mid[F, Option[Output]] =
      for {
        _ <- info"getLatest()"
        r <- _
        _ <- info"getLatest() -> $r"
      } yield r

    def setPredicted(dexyOutput: Output): Mid[F, Unit] =
      for {
        _ <- info"setPredicted(${dexyOutput.boxId})"
        r <- _
        _ <- info"setPredicted() -> success"
      } yield r

    def invalidateAndUpdate: Mid[F, Unit] =
      for {
        _ <- info"invalidateAndUpdate()"
        r <- _
        _ <- info"invalidateAndUpdate() -> success"
      } yield r
  }
}
