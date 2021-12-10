package org.ergoplatform.dex.markets

import cats.effect.{Blocker, Resource}
import cats.tagless.syntax.functorK._
import org.ergoplatform.common.EnvApp
import org.ergoplatform.common.db.{PostgresTransactor, doobieLogging}
import org.ergoplatform.dex.markets.configs.ConfigBundle
import org.ergoplatform.ergo.ErgoNetworkStreaming
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.SttpBackend
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import tofu.WithRun
import tofu.doobie.instances.implicits._
import tofu.doobie.log.EmbeddableLogHandler
import tofu.doobie.transactor.Txr
import tofu.fs2Instances._
import tofu.logging.Logs
import tofu.syntax.unlift._
import zio.interop.catz._
import zio.{ExitCode, URIO, ZEnv}

object App extends EnvApp[ConfigBundle] {

  def run(args: List[String]): URIO[ZEnv, ExitCode] = ???

  private def resources(configPathOpt: Option[String]): Resource[InitF, ConfigBundle] =
    for {
      blocker <- Blocker[InitF]
      configs <- Resource.eval(ConfigBundle.load[InitF](configPathOpt, blocker))
      trans   <- PostgresTransactor.make("markets-api-pool", configs.pg)
      implicit0(xa: Txr.Contextual[RunF, ConfigBundle]) = Txr.contextual[RunF](trans)
      implicit0(elh: EmbeddableLogHandler[xa.DB]) <-
        Resource.eval(doobieLogging.makeEmbeddableHandler[InitF, RunF, xa.DB]("matcher-db-logging"))
      implicit0(logsDb: Logs[InitF, xa.DB]) = Logs.sync[InitF, xa.DB]
      implicit0(backend: SttpBackend[RunF, Fs2Streams[RunF]]) <- makeBackend(configs, blocker)
      implicit0(client: ErgoNetworkStreaming[StreamF, RunF]) = ErgoNetworkStreaming.make[StreamF, RunF]
    } yield configs

  private def makeBackend(
    configs: ConfigBundle,
    blocker: Blocker
  )(implicit wr: WithRun[RunF, InitF, ConfigBundle]): Resource[InitF, SttpBackend[RunF, Fs2Streams[RunF]]] =
    Resource
      .eval(wr.concurrentEffect)
      .flatMap(implicit ce => AsyncHttpClientFs2Backend.resource[RunF](blocker))
      .mapK(wr.runContextK(configs))
}
