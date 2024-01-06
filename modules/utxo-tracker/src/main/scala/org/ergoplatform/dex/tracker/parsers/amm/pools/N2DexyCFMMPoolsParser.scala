package org.ergoplatform.dex.tracker.parsers.amm.pools

import org.ergoplatform.dex.domain.amm.{CFMMPool, PoolId}
import org.ergoplatform.dex.domain.{AssetAmount, BoxInfo}
import org.ergoplatform.dex.protocol.amm.AMMType.N2Dexy_CFMM
import org.ergoplatform.dex.protocol.amm.constants
import org.ergoplatform.ergo.domain.Output

object N2DexyCFMMPoolsParser extends CFMMPoolsParser[N2Dexy_CFMM] {

  def pool(box: Output): Option[CFMMPool] = {
    val feeNum = 997
    for {
      nft <- box.assets.lift(constants.cfmm.n2dexy.IndexNFT)
      lp  <- box.assets.lift(constants.cfmm.n2dexy.IndexLP)
      y   <- box.assets.lift(constants.cfmm.n2dexy.IndexY)
    } yield CFMMPool(
      PoolId(nft.tokenId),
      AssetAmount.fromBoxAsset(lp),
      AssetAmount.native(box.value),
      AssetAmount.fromBoxAsset(y),
      feeNum,
      BoxInfo.fromBox(box)
    )
  }
}
