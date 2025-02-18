package org.ergoplatform.dex.executor.amm.config

import derevo.derive
import derevo.pureconfig.pureconfigReader
import org.ergoplatform.ergo.TokenId
import tofu.Context
import tofu.logging.derivation.loggable

@derive(pureconfigReader, loggable)
final case class DexyConfig(swapTokenId: TokenId, depositTokenId: TokenId, redeemTokenId: TokenId)

object DexyConfig extends Context.Companion[DexyConfig]
