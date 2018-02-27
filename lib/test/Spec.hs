-- Unfortunately, we can't use the hspec-discover tool, because our Specs
-- need the magic Data.Reflection Has*Configuration constraints.
-- hspec-discover only works if all of the Specs have type Spec.
--{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Spec #-}

module Spec (spec) where

import           Test.Hspec (Spec, describe)

import           Test.Pos.Configuration (HasConfigurations)

import qualified Test.Pos.Block.Identity.SafeCopySpec as Block.Identity.SafeCopy
import qualified Test.Pos.Cbor.CborSpec as Cbor.Cbor
import qualified Test.Pos.ConstantsSpec as Constants
import qualified Test.Pos.Core.AddressSpec as Core.Address
import qualified Test.Pos.Core.CoinSpec as Core.Coin
import qualified Test.Pos.Core.SeedSpec as Core.Seed
import qualified Test.Pos.Core.SlottingSpec as Core.Slotting
import qualified Test.Pos.CryptoSpec as Crypto
import qualified Test.Pos.Genesis.CanonicalSpec as Genesis.Canonical
import qualified Test.Pos.Lrc.FollowTheSatoshiSpec as Lrc.FollowTheSatoshi
import qualified Test.Pos.MerkleSpec as Merkle
import qualified Test.Pos.Slotting.TypesSpec as Slotting.Types
import qualified Test.Pos.Ssc.ComputeSharesSpec as Ssc.ComputeShares
import qualified Test.Pos.Ssc.Identity.SafeCopySpec as Ssc.Identity.SafeCopy
import qualified Test.Pos.Ssc.SeedSpec as Ssc.Seed
import qualified Test.Pos.Ssc.Toss.BaseSpec as Ssc.Toss.Base
import qualified Test.Pos.Ssc.Toss.PureSpec as Ssc.Toss.Pure
import qualified Test.Pos.Ssc.VssCertDataSpec as Ssc.VssCertData
import qualified Test.Pos.Txp.CoreSpec as Txp.Core
import qualified Test.Pos.Txp.Toil.UtxoSpec as Txp.Toil.Utxo
import qualified Test.Pos.Types.BlockSpec as Types.Block
import qualified Test.Pos.Types.Identity.SafeCopySpec as Types.Identity.SafeCopy
import qualified Test.Pos.Types.Identity.ShowReadSpec as Types.Identity.ShowRead
import qualified Test.Pos.Update.Identity.SafeCopySpec as Update.Identity.SafeCopy
import qualified Test.Pos.Update.MemStateSpec as Update.MemState
import qualified Test.Pos.Update.PollSpec as Update.Poll
import qualified Test.Pos.Util.LimitsSpec as Util.Limits
import qualified Test.Pos.Util.ModifierSpec as Util.Modifier
import qualified Test.Pos.UtilSpec as Util

spec :: HasConfigurations => Spec
spec = do
    describe "Block.Identity.SafeCopy" Block.Identity.SafeCopy.spec
    describe "Cbor.Cbor" Cbor.Cbor.spec
    describe "Constants" Constants.spec
    describe "Core.Address" Core.Address.spec
    describe "Core.Coin" Core.Coin.spec
    describe "Core.Seed" Core.Seed.spec
    describe "Core.Slotting" Core.Slotting.spec
    describe "Crypto" Crypto.spec
    describe "Genesis.Canonical" Genesis.Canonical.spec
    describe "Lrc.FollowTheSatoshi" Lrc.FollowTheSatoshi.spec
    describe "Merkle" Merkle.spec
    describe "Slotting.Types" Slotting.Types.spec
    describe "Ssc.ComputeShares" Ssc.ComputeShares.spec
    describe "Ssc.Identity.SafeCopy" Ssc.Identity.SafeCopy.spec
    describe "Ssc.Seed" Ssc.Seed.spec
    describe "Ssc.Toss.Base" Ssc.Toss.Base.spec
    describe "Ssc.Toss.Pure" Ssc.Toss.Pure.spec
    describe "Ssc.VssCertData" Ssc.VssCertData.spec
    describe "Txp.Core" Txp.Core.spec
    describe "Txp.Toil.Utxo" Txp.Toil.Utxo.spec
    describe "Types.Block" Types.Block.spec
    describe "Types.Identity.SafeCopy" Types.Identity.SafeCopy.spec
    describe "Types.Identity.ShowRead" Types.Identity.ShowRead.spec
    describe "Update.Identity.SafeCopy" Update.Identity.SafeCopy.spec
    describe "Update.MemState" Update.MemState.spec
    describe "Update.Poll" Update.Poll.spec
    describe "Util.Limits" Util.Limits.spec
    describe "Util.Modifier" Util.Modifier.spec
    describe "Util" Util.spec
