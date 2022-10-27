package org.ergoplatform.dex.index.sql

import doobie.{Fragment, Query0}
import doobie.implicits._
import doobie.util.log.LogHandler
import org.ergoplatform.common.sql.QuerySet
import org.ergoplatform.dex.domain.amm.PoolId
import org.ergoplatform.dex.index.db.models.{LiquidityProviderSnapshot, PoolSnapshot, SwapAvg, UnresolvedState}
import org.ergoplatform.ergo.{PubKey, TokenId}

object LiquidityProvidersSql extends QuerySet[LiquidityProviderSnapshot] {

  val pools: Map[TokenId, PoolId] = Map(
    "64c8bf656ccf18f4d1c54db3dc842421861c0af9ce980645e13a40fc309406e8" -> "09586882b55e04a772a5fc20e2fe33e98772257e376a61309b90129381cf5ccd",
    "a7f272af0be99b2a14fe69448031340fe3a8869062767f9670c7184ae032b582" -> "13f31cd8ee3988e539dfcb40790b3e820e4cd85c9a74fcaade126225c9713bc9",
    "1f2d9bfbf99f6ef9b259115dfe6bc83252855e76f9b43896f53e1dc190cfa8da" -> "bee300e9c81e48d7ab5fc29294c7bbb536cf9dcd9c91ee3be9898faec91b11b6",
    "e249780a22e14279357103749102d0a7033e0459d10b7f277356522ae9df779c" -> "7d2e28431063cbb1e9e14468facc47b984d962532c19b0b14f74d0ce9ed459be",
    "d8d430aabf5b66f1251a8ac7fade330e1c70083da66880291dcd868579d3cdae" -> "9467db25390ab502abfba54fe27f7fece72d5120935d66bafdb0bf18d573b42c",
    "66ed74249c2c6ba0f6d2cd80f0a99ac8690b03e11c46860141ab7099d7308fbd" -> "e40647533280058e640a28fb917fd9e73d5ecccc8be97f38aefe7315739f30ac",
    "fdaf938289ed12a55754fd9813098e4f9fdeffc2b6a59f20ed0a119977865407" -> "e16af68ac3efb6a5a51ee12dadec737979cc7b1ded0a2ba67a83b7d56b73348f",
    "fa6326a26334f5e933b96470b53b45083374f71912b0d7597f00c2c7ebeb5da6" -> "1d5afc59838920bb5ef2a8f9d63825a55b1d48e269d7cecee335d637c3ff5f3f",
    "3499bb332db42a22784606d2dab6ff15e4352699584c1af942a9b7a5af836986" -> "1f01dc8e29806d96ca0b79f8e798cd8cfce51c0e676aaedf6ab3464b37da9dfd",
    "28a18a23c3a96241d8dff7cbef89b8591d1238e8065190e5b57c5024ef5f855f" -> "67d235654ee562ce841c2e10be1ad6ea98b13d3b2bec0b3b1104ea2bd2e82ba8",
    "7f0c4d34f9eb9d6755b49499109691a5baecb29ff4f2c01345bfe0e147e2c484" -> "10cc76db30a0a6bcdb7a3c27221c4b6a7ef7a05ab43bee9fe5e76aafac40126c",
    "5f96f3570acb5364dfc13b0b665ebd526c2459baf58044547f2eb48a45ca7df3" -> "7601494c813a7b1acbdecc042d9eb337ea79a556221c9567685f82077cea7b20",
    "69565eb9c2a90c6c4ba54340c8bb9342e6bcfc8e6fce640fd626acdd94225ee6" -> "811592b58cafe75a99c8416637d427e756aeb1930eceb91cc1cf3c2c433e310e",
    "1d3d1e9534d67ca76d3e6d221439acffad51ee689bb4317b1047b054a336d2eb" -> "47d68f8a400798230bd0620ead41ab608dffad29d104bcfde8d8a23c19fc61f7",
    "b8f51b9f35c0e778ea4c89c6ff6fefd91f5471ed8eac57be19370ddef23e1127" -> "d5ef37369294ddb00155b40079b80ca91a2fb1be7b0578b5e80eb822b1467388",
    "985e486165e585c0a146389da5e222abcb9f76b3055adca87c5f43427ef48ce6" -> "51927a4f5a175e5c532219cbd7790726abcf774ef36c997e915e9b3603250b6e",
    "05d8fdb5ec02279beee1872655aa400a791be2929cee7ec42dc465a0a211dadc" -> "375244938b0cda84b59f8fe7c5328b51a3b0b2ef6ae1eda0d8b4bc25eb6be7fa",
    "96c559f706c266da43d7524a29624970c9a33c6a64d6ecaab56f1030619575cf" -> "94e7c5993db6bf42ec4473379f12194edab6eb0e858b3c256efd69fd786ef72a",
    "1256f5cd95a429f41764be154f1928cfd6e4fbacc3655ceb193e06c7fa5fafd5" -> "2d97a2b2385ca12f0614578ab49db2ebf8f0c14ba634298c943ec24658c7b052",
    "b7d82eb7889cbb2b58cb6328599a839167e3ee067362e2cedbc49e9185f31510" -> "cab1a69b6d9921a0d528055c9ce3c0c7b84f7c0d6ad6ed77caf958890bdb11d3",
    "3ae312dcf8dfe532bd1105dea6ed3a611b41e4b77e5e793d224cda869c47c9ab" -> "248bd75380f0330cc5e3301ad40c3dd8c375e1316e52fd53abbe5674b43658d0",
    "f3af2ee5cdd5d21f14402ac80a9b633eec1e9052b02d27d3601afc17943bd121" -> "619904a79a539c9dc1ca40fa8fb2be1c336e9bab9096a5ec2eabf38554d10f29",
    "19058e11389e4f22832528b9bbff087c3cf46d230e570f46ccb6bc24a149fcd0" -> "4686df5219f85de7e849cf053dffc5503b533d9d5febc5b45d9253247829fdb5",
    "6f6c91f65f198261397e960cecb699fa498bab002502b7b108ca4590150a221f" -> "ddcfb0239162a95ebd9265563d58366693882582ba87ad4d1a29425596936ae1",
    "e7021bda9872a7eb2aa69dd704e6a997dae9d1b40d1ff7a50e426ef78c6f6f87" -> "04f468174eddbc68bce3f0965dd14bc6ed1443f5a405ec7f7f9925d999370b97",
    "17d3858be511bb3fe7d12e6e0a3ba896627ebb5cc5a2cfc347a1c201c6f905f9" -> "76ce3302854017274bdd91f83db4c8a84b43cf22ba15f7f59764e3096d205cab",
    "aeba46d6d583d9eba2436c22f872c12f0423a740d37b0950fe6b3e8f30f8e050" -> "f36963017275b4e917770812594fa8e8c9a6c47a2bf3aff1df8f2b644d7ba864",
    "16134704f2452673ad493486acecb11d0d131e121971a72a6e6c715d88f3e478" -> "9679d44a1a0a88e2d9d3d9c8a4e5302cb1fc128da91e80088661fba98a616519",
    "88d61bf95b1ca83ed0b008ab889cb531e550f4a89c09385b6bb18e583ab88055" -> "c0070feda814a6d74f97ac24444976a4d07aae9d2645874732df99e94617bfdb",
    "303f39026572bcb4060b51fafc93787a236bb243744babaa99fceb833d61e198" -> "9916d75132593c8b07fe18bd8d583bda1652eed7565cf41a4738ddd90fc992ec",
    "f83f1284f54d4525507013fd399885cbcafd6ad457de976ba512ac65299d37d1" -> "5d8f7082a2f8fad8788b4117ed9e54a72d58bde786e1267d3d87745a14629d6d",
    "cf8d83a2803b99f4fbb596b29014a1e6b1e31c7d0817f2767617f1f6b1438a1e" -> "61a579c46d92f2718576fc9839a2a1983f172e889ec234af8504b5bbf10edd89",
    "22f4242e503e929f1fda86bb032ea3725bb0a447ceebf87a7f61122327bc4a13" -> "54924d4d283c2c9f9afde2f39577d28836f919469ab72d4e373ba167065ad798",
    "879c71d7d9ad213024962824e7f6f225b282dfb818326b46e80e155a11a90544" -> "666be5df835a48b99c40a395a8aa3ea6ce39ede2cd77c02921d629b9baad8200",
    "98da76cecb772029cfec3d53727d5ff37d5875691825fbba743464af0c89ce45" -> "fff5e73c7c8c61335d8c4ab18a2d09029e1c9131ed195c4e0ceccf9dd0c03d7f",
    "6fe49e775701a342054685a16c82c930c1bfe30c3591d1bfecf35d4bc035e933" -> "c4d0af619679116d126866356a717d4024b59632e6320c60c37c7a44f97f2d22",
    "fad6deb9b84314e097cc0942e434b3a5148ef539cc1f5793ac5aa12570cd93e5" -> "e3989fdc13d47e2113a7d421d6860325755426c5b582ff738f8d0d6efa618f4e",
    "684a530b755fe9b18e66be60b833f127c7899a207f880a19238cd4d7fba04e1e" -> "389f7fe77a53f787fd68c1e244e88b230c68d720a4b580057eb904db3d3c8815",
    "362a124a709215c6c7db07db578de3682da2b0c912174a7cf609349061415165" -> "931f133cdd56902cff8870a71cd8562fd9721037b5c713a516fbdda976ba1cf0",
    "7b142af2d77b6983b37a8c426ae52cf9ee084f601e06033085f74a553bc1bfaa" -> "88e0165da93412a5963fdfc80b6c9030ad32e8ec244e6d7d9a736d8870b19cca",
    "751043812a5c10a2c707a4f51ee3dea1d45be8e39f6435bfd867526c10411c96" -> "beed0c492a92f48af41d5496522b01f973d80d2bf58ca0696125dc543ee651b5",
    "22c5117365c055428637da4a65ecd74068050baf9d3f7765ed386381e6e5b385" -> "19e17567d43afb16519d2dad3522a3f3a25c71ac97812f429ab5eeb73cbb62d1",
    "b322311a38532addc7b8ba58436f5f50a36457335896544bdd5768653ac9dc38" -> "01ca8d2ba98469014b4112df291e44d6bd0455dd47ca261ff1a26bc514ee4746",
    "6b56392067f791b99d31488d013d077f8fba76ed1af452794dd1cf071f76ff60" -> "fdb6af4ba41646c987666db47ff14ae06e587dc2752266ffe99906df62a23db8",
    "9db3450951c37a5875ed493f19bfe03364fa4846cde63b1fee05dfaf59c837b1" -> "c1b9c430249bd97326042fdb09c0fb6fe1455d498a20568cc64390bfeca8aff2",
    "26e4575201b3504dcd765b3ecb385e17ba84b0ebfea201ae8477b9b8592f7ab6" -> "647ab23eb8ad9aabb21b64bdee46d47b7e21d85cc6c3c5b160ca5a7b32815e9f",
    "8f3698b213217011529b7ccb78aca9bd01bb349c434d12830b1584eb83856e0d" -> "4c1638990e9afa7b13e3e0f91a9aee538ad70113591d64425192f54b3377ddec",
    "605d9562cc9b9d79400fb70f95d14519b4b63c92000737b6ca875b58c2de07f4" -> "e24d17f85ac406827b0436a648f3960d8965e677700949ff28ab0ca9a37dd50e",
    "660015ebe4666151171d58b4235c8a1a6183cf3e73458e254cc3d14ff9a66ba3" -> "9c1d78e53e7812df96bbb09b757ee1e059c5a298d85789b5c82a7222c34e8f61",
    "4aa8c1c03ce3108f06f82e9a24d511e39c176dc7ee5e8d068c81ab2464ceee49" -> "5af3aed2568d345b5415cb299772d15c4483755f94a77aa4b0eb5abd2c2faa46",
    "8f4a21d2426e373635da5da510fbd3220aa57f754a18ea0ee647df08a4537584" -> "a62a1603bae6c0d293f7b954672de6fa8eae9793d433cb02ee49dce736da54ac",
    "91087623aaadb04bd49edf31453f4fdfd036e6aafc792979d95d17509870540e" -> "c8f0a31d4d732ca3c2013bd200a91ee31dcdf76ffe81bec277e9ea005b684547",
    "e5ccc679c0d2911bac223b51c6f78977c9818eb2b1c2982be057a7e41f112b71" -> "4ad32364b11b0fc1cc66e6528b5ad8cbc31f2f63793357316bf15c8ef283ad4c",
    "90121b816f1fc97d789dcebc89a8b47dec05a2279a731dd8c08656187c380324" -> "10cda33aebb7c480a0944aeb8e0ba9738cb6ac6d9848fb8df6ad0cc059e2028a",
    "174e96e6b66deecc5dfa66e596c0bd2a7072fefef7667d0936618a88e7d7de21" -> "f6fd773ec289c3177645d90be61118d81bcc9c5d4e7730069d0b9ecb0a543e05",
    "7b6937be6b6c5ceb55656eda6d8f1958d90acb1c2ab57bef401561cba7fea9f0" -> "fcbb10ab3905601b9638b5c50e3e7faaf2d31d5cab28ac4b2a913b51136bc55d",
    "217bbc196f1f50a7e7e8e2ea483813246583f90b2f35df8197f544692b22fc5b" -> "80578f5b9854002cd964aef5e44e12c9558c4e9c5ce199f5a9d42c0583be5fb1",
    "07ffe09a58d66c876d1be78936cd0e70bddb61c920f055e7bda98cb21b789709" -> "51acb3c1f56af84ccb780bc2d232a6a5f994280c4e184352d44c749c2895b242",
    "c319c2671c6e67f2b0e0aa5b03d2f023cdf8abb654f3b47f0493b6a377bcabd1" -> "03b695cd465285b05ce99755046e93910f78dfa869b444427cf1a10388d4aea5",
    "bc1813b933672629e639d91fa66578890dc5bbab064ee4e516cad2c5fa72019c" -> "ea6d2ceff1565ac062cb260f558f1ed492dcd805dcd83fd1ecaf0e0407c8f1ff",
    "1ac77b7a8987a5fc87c3a10140af01bf59d3655bbcdb801860eda9048d04b843" -> "a7d1d136d0c12fd53e7135c546eef9b2d2d0fbff43198a583f75c664760cca23",
    "c95645836ca45f6923978e175b93305185406aa939b213c96e44b6645911d04f" -> "3d4fdb931917647f4755ada29d13247686df84bd8aea060d22584081bd11ba69",
    "6482070e90175ec4ba3ae91cf0bfc638a20e6c01759f686775ee6f08d475d6c6" -> "9afb06747a82f682138f8f877a01e3d72da043d1eb8767905956a60c7b853a2e",
    "000db3f3b71ad1cfc1327ec9e132120096e24cf720f5ef6ecae897dfe72b1aa9" -> "3b81cea6c83063ccfa1e890c8be5a8b4aa11c0771e2cc8f1150c2eeadab55ef6",
    "fdb2f0c5f92d0df29b98ddfd69e139da71917095344122030bef14ac2835c886" -> "53ac87304709dd4a697e38b52ea0d2e862e3827415809230f20bc580baee9586",
    "0c4cd12252b8bc378be53a28a5ff0ca143b486f9a9e97d3bad583b9c92c25983" -> "643068568273d28c9588e658d8b86bc888644fb945b11ac3615eda2c1306888e",
    "87125f824386ad8cb65f8c047ed4f4fc8b95142d9816ac505fe32869f8c69f82" -> "945747ffc0b105449845b557e4b3d997eef95ea91e9afc58deb3c217098c980b",
    "f7cf16e6eed0d11ffd3f55186e00085748e78f487cb6e517b2f610e0045509fe" -> "d7868533f26db1b1728c1f85c2326a3c0327b57ddab14e41a2b77a5d4c20f4b2",
    "180d140bd03db17009acd151107749b2d917ac48108217a9d9253bab959e885a" -> "47101cd1b5a4260f6d51b0c756e8b4ff5d3ebc3916a041b9fed9b072dbf416cd",
    "4c2c215a7ef086c43f6144ccded8ca34d3e4dc39e38f2c133e638303acbd4e19" -> "341f1f8136f672384aa0b7f38989d54f7fb5a530b760598d39646a4e85c1a623",
    "7e8c3131664ef191a09051705e99d2227943cbd5832975c3ebdc15d65ac42410" -> "a8a2275ff1660b6ceb6e4c977f4359c6c51b53b0a4e0f446496108770ed6a893",
    "20215f1905741b85251c3bf02210821820068f7487d12489f20eb1374256fe80" -> "e59494b2056a3b05ccae7f01c9e9bba4e750fa452bc7835ef5ce5e2d88dbef44",
    "22cdfb8e11795f3ff59f672809b61e3c2908cd5e81d72d027620ef42f7276e9e" -> "ab80ad4c46d7cdb2ad10d5a61a93166878654ec900de0fbd503f867b2156a91a",
    "ee1c20cd176f306dd0cde48c6c8b2a891d0726c7a6b1b76a7938db5fac496e03" -> "31f4b2b5fc721efd864e51582f99e6fcec73fe0fe859f2e416354fc97e738f78",
    "b98f1b184a911dfa4aae08a7ffbd41680c5c503f8a3bd1937b5c4f0f36218f14" -> "dc234b4f8e7844fbd2a6c10f41b2e51c5422341b8648e83ed0295e5eb3f40913",
    "273f425c1caa443d223f88cec0328d3669aed4a7e71b77748e65e585ab8648ff" -> "f709d33db375699d8f284a9f7abfb0b1a36139aa5f67945f7a231bd424a70a62",
    "e6ed2270b6a3443c4203a086bd2010a65cda3ba82c8eb31e8e622a7de35cc086" -> "9eccc846606c4a34e2e3ac189041a8f41e3dc1681f61bb0c5e1f6d3cb9c380af",
    "1e97a1948a9434d55be3a66edb8a877c8d7dd9c1a6caaeb29f29047359436c44" -> "1574dde8b5a36b1a6a5745542365554fd666ab8fc7c013f7d60a72eeaeacadaa",
    "12fcf147f4fe0f0661af79e51e20e76bd1ef6c67a5792ea04b0b60e52a82de28" -> "80b459491c5ff10d2a7e8c5802d2e88dd068e58cad39fe8bd21ee16f5f3eb98b",
    "566acbe1657136edc3bd7c27f1619490ed9a896f9dc7dbea07ace755c4ff857a" -> "27d84d3f90c8b335f4cad9de170cc5c1ba837ef71282eb2e427582c86d8356ea",
    "c82b31e7eaf67ab7cb5d84d99f5cd92d90c2cb1c598bd0b4def8b175e692bcfb" -> "86471f2b5ee47e0ae0d41b1694b7a419e0550c2c5d16282f78625c8c7523c81c",
    "f35c15ebda13c1e3b1125b277a9a103fb45146b92be10b5cfd1e97ecc3afc784" -> "62adfb20a1b6c47c37b06c4703e2ebe984b1448b9fe1df258ef4d4622ca4c3f8",
    "b709a56d0b5a9e0f9a6291e07b9ac6c86e17c3383a9ca4aa2b3fba3e27f39a31" -> "96919e9f787bc68fedcddcb76ccf21a781171b86a959a99a2844f0cb6c18f76e",
    "88f590090e19fc3e3005cd1fede2ded3cf3f65fdfc4b28bc8f0378986fd87a5a" -> "d8e2a257e8cb97139ddb3f6e288bfc84cc4ca8194c83d697c0c6ca7118f26bc8",
    "12ef14a43bab03dbb3a9b066d073ddba06493d9fbb1a8521c2f75205ba275663" -> "61ae855c62440c0ec8429f1a31d956d9aaf9d8727b89cc26fd894126d9cbce06",
    "88eac61a302e79dfdfea6f15ff4b9a92cfe4252f8ead70dda208447fb542747b" -> "0b36eb5086ba1d258341723fa4768acaa3804fba982641a00941d5aad2107f50",
    "05998fa93926a1b953c0ba83c7d491eb328022a393d5324e598ce0777a3404a1" -> "fd4c08b0f40682b9c0fa6a51987b5bf3286dbfe6e71249940a7d150587a22bff",
    "453e748351262e29366de5d58b502bfe59d21001e7a0757589a5c7afae12f85b" -> "4b5541bef755585f681d9c9bbf06f3ead9dd69ab120f5ab87094d26ca8057b94",
    "c904df22e48423eea228dc750eaa2665a2653fde7acf3f35b1f1537ce6644fad" -> "b6b38cae74e4754ae70a7c4335a9150449e47cb7421394016ab73c5c22a1a9dc",
    "c46169d172471b5619d3aa61616e7ee6f07a81fd9c9e2db21fb4e61473ed967b" -> "edca8164231dccffeacf4d923267e91aa192961c38c815ec3b9893d93dc629df",
    "9af53b9c6059c6d727bcd906b9248d319f27d3e5f7056031e226fe2c07f95e5f" -> "b699c3d98e3488bff8999ef205e6be719a6a3de32771318aabb4af60b6d6c654",
    "337bff9e2156ac5d0c6da727cf74bacf49486097c3b237b6c45cbb010f25dd61" -> "6e55937e9b6fc4b6bae36726eed3f44c47e705aec1e0acdf01876eb0198fc075",
    "2288a35ba8b821e304078b2c3e24ac80e1a68fe40fd2306749d3086c507e7d2d" -> "3d648a56fc4749fe7822a25c0abe6afba7965885880815ad4bfafae5dca7237f"
  ).map { case (k, v) =>
    TokenId.fromStringUnsafe(k) -> PoolId.fromStringUnsafe(v)
  }

  val sPools = Fragment.const(pools.values.toList.map(_.unwrapped).map(s => s"'$s'").mkString(", "))

  val tableName: String = "state"

  val fields: List[String] = List(
    "address",
    "pool_id",
    "lp_id",
    "box_id",
    "tx_id",
    "block_id",
    "balance",
    "timestamp",
    "weight",
    "op",
    "amount",
    "gap",
    "lpErg",
    "txHeight",
    "poolStateId"
  )

  def getLatestLiquidityProviderSnapshot(address: String, poolId: PoolId)(implicit
    lh: LogHandler
  ): Query0[LiquidityProviderSnapshot] =
    sql"""|select address, pool_id, lp_id, box_id, tx_id, block_id, balance, timestamp, weight, op, amount, gap, lpErg, txHeight, poolStateId from state
          |where address = $address and pool_id = $poolId order by id desc limit 1
          """.stripMargin.query[LiquidityProviderSnapshot]

  def getLatestPoolSnapshot(poolId: PoolId, to: Long): Query0[PoolSnapshot] =
    sql"""
          |select lp_amount, x_id, x_amount, y_id, y_amount, pool_state_id from pools
          |where pool_id = $poolId and height <= $to
          |order by height desc limit 1;
       """.stripMargin.query

  def getAllUnresolvedStates: Query0[UnresolvedState] =
    sql"""
         |select s1.address, s1.pool_id, s1.lp_id, s1.balance, s1.lperg, s1.timestamp, s1.box_id from state s1
         |  left join (
         |    select address, pool_id, max(id) as id
         |    from state
         |    where balance::decimal != 0 GROUP by address, pool_id
         |  ) s2 on s2.id = s1.id
         |where s1.id = s2.id"""
      .stripMargin.query

  def selectAllSwapUsers: Query0[PubKey] =
    sql"""
         |SELECT
         |	redeemer
         |FROM
         |	swaps
         |WHERE
         |	output_amount IS NOT NULL
         |	AND timestamp > 1633910400000
         |	AND pool_id in($sPools)
         |GROUP BY;
       """.stripMargin.query

  def getDaysOfSwapsByAddress(key: org.ergoplatform.ergo.PubKey): doobie.Query0[SwapAvg] =
    sql"""
         |SELECT
         |	to_char(to_timestamp(timestamp / 1000) at time zone 'UTC', 'YYYY-mm-dd') ts,
         |	sum(
         |		CASE WHEN input_id = '0000000000000000000000000000000000000000000000000000000000000000' THEN
         |			input_value
         |		ELSE
         |			output_amount
         |		END),
         |	count(1)
         |FROM
         |	swaps
         |WHERE
         |	redeemer = $key and pool_id in ($sPools) and output_amount is not null and timestamp > 1633910400000
         |GROUP BY
         |	ts;
     """.stripMargin.query[SwapAvg]

}
