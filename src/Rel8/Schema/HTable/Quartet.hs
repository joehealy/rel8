{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Quartet
  ( HQuartet(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude

-- rel8
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Bifunctor ( HBifunctor( hbimap ) )
import Rel8.Schema.HTable.Functor ( HFunctor( hmap ) )


type HQuartet :: HKTable -> HKTable -> HKTable -> HKTable -> HKTable
data HQuartet w x y z context = HQuartet
  { hfst :: w context
  , hsnd :: x context
  , htrd :: y context
  , hfrt :: z context
  }
  deriving stock Generic
  deriving anyclass HTable


instance HBifunctor (HQuartet a b) where
  hbimap f g (HQuartet a b c d) = HQuartet a b (f c) (g d)


instance HFunctor (HQuartet a b c) where
  hmap = hbimap id
