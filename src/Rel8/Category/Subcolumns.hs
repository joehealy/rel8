{-# language DataKinds #-}
{-# language ExistentialQuantification #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Category.Subcolumns
  ( Subcolumns( Subcolumns )
  )
where

-- base
import Control.Category ( Category, (.), id )
import Data.Kind ( Type )
import Prelude hiding ( (.), fst, id, snd )

-- categories
import Control.Categorical.Bifunctor
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Cartesian ( Cartesian, Product, fst, snd )
import Control.Category.Monoidal

-- rel8
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable.Label
import Rel8.Schema.HTable.Product
import Rel8.Table ( Columns, Context )


type Subcolumns :: Type -> Type -> Type
data Subcolumns a b = Context a ~ Context b =>
  Subcolumns (Columns a (Col (Context a)) -> Columns b (Col (Context b)))


instance Category Subcolumns where
  id = Subcolumns id
  Subcolumns f . Subcolumns g = Subcolumns (f . g)


instance PFunctor (,) Subcolumns Subcolumns where
  first (Subcolumns f) = Subcolumns $ \(HProduct a b) ->
    HProduct (hlabel (f (hunlabel a))) b


{-
instance QFunctor (,) Subcolumns Subcolumns where


instance Bifunctor (,) Subcolumns Subcolumns Subcolumns where


instance Associative Subcolumns (,) where


instance Braided Subcolumns (,) where


instance Monoidal Subcolumns (,) where


instance Symmetric Subcolumns (,) where


instance Cartesian Subcolumns where
  type Product Subcolumns = (,)
-}
