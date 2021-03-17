{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Expr.Array
  ( listOf, nonEmptyOf
  , sappend, sempty
  )
where

-- base
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye ( sfromPrimExpr, stoPrimExpr )
import Rel8.Kind.Blueprint ( SBlueprint( SVector ), ToDBType )
import Rel8.Kind.Emptiability
  ( Emptiability( Emptiable, NonEmptiable )
  , SEmptiability( SEmptiable )
  )
import Rel8.Kind.Nullability ( Nullability( NonNullable ), SNullability )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Array ( Array, array )


sappend :: a ~ ToDBType blueprint
  => SEmptiability emptiability
  -> SNullability nullability
  -> SBlueprint blueprint
  -> Expr nullability' (Array emptiability nullability a)
  -> Expr nullability' (Array emptiability nullability a)
  -> Expr nullability' (Array emptiability nullability a)
sappend emptiability nullability blueprint a b =
  sfromPrimExpr blueprint' $
    Opaleye.BinExpr (Opaleye.:||)
      (stoPrimExpr blueprint' a)
      (stoPrimExpr blueprint' b)
  where
    blueprint' = SVector emptiability nullability blueprint


sempty :: a ~ ToDBType blueprint
  => SNullability nullability
  -> SBlueprint blueprint
  -> Expr nullability' (Array 'Emptiable nullability a)
sempty nullability blueprint = sfromPrimExpr blueprint' $ Opaleye.ArrayExpr []
  where
    blueprint' = SVector SEmptiable nullability blueprint


listOf :: forall a nullability. DBType a
  => [Expr nullability a]
  -> Expr 'NonNullable (Array 'Emptiable nullability a)
listOf = Expr . array (typeInformation @a) . fmap (\(Expr a) -> a)


nonEmptyOf :: forall a nullability. DBType a
  => NonEmpty (Expr nullability a)
  -> Expr 'NonNullable (Array 'NonEmptiable nullability a)
nonEmptyOf = Expr . array (typeInformation @a) . fmap (\(Expr a) -> a)
