{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB, unDB )
  , Insert( RequiredInsert, OptionalInsert )
  , Name( Name )
  , Labels( Labels )
  , Result( Result )
  , IsSpecialContext
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.List.NonEmpty ( NonEmpty )
import Data.String ( IsString )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( Aggregate )
import Rel8.Kind.Blueprint ( ToDBType, ToType )
import Rel8.Kind.Necessity
  ( Necessity( Optional, Required )
  , SNecessity( SOptional, SRequired )
  , KnownNecessity, necessitySing
  )
import Rel8.Kind.Nullability ( KnownNullability )
import Rel8.Schema.Spec ( Context, Spec( Spec ) )
import Rel8.Schema.Structure ( Structure )
import Rel8.Schema.Value ( Value )
import Rel8.Type.Monoid ( DBMonoid )
import Rel8.Type.Semigroup ( DBSemigroup )


type Aggregation :: Context
data Aggregation spec where
  Aggregation :: ()
    => Aggregate nullability (ToDBType blueprint)
    -> Aggregation ('Spec necessity nullability blueprint)


type DB :: Context
data DB spec where
  DB :: ()
    => { unDB :: Expr nullability (ToDBType blueprint) }
    -> DB ('Spec necessity nullability blueprint)


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , DBSemigroup (ToDBType blueprint)
  ) =>
  Semigroup (DB spec)
 where
  DB a <> DB b = DB (a <> b)


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , DBMonoid (ToDBType blueprint)
  ) =>
  Monoid (DB spec)
 where
  mempty = DB mempty


type Insert :: Context
data Insert spec where
  RequiredInsert :: ()
    => Expr nullability (ToDBType blueprint)
    -> Insert ('Spec 'Required nullability blueprint)
  OptionalInsert :: ()
    => Maybe (Expr nullability (ToDBType blueprint))
    -> Insert ('Spec 'Optional nullability blueprint)


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , DBSemigroup (ToDBType blueprint)
  ) =>
  Semigroup (Insert spec)
 where
  RequiredInsert a <> RequiredInsert b = RequiredInsert (a <> b)
  OptionalInsert ma <> OptionalInsert mb = OptionalInsert (liftA2 (<>) ma mb)


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , KnownNecessity necessity
  , DBMonoid (ToDBType blueprint)
  ) => Monoid (Insert spec)
 where
  mempty = case necessitySing @necessity of
    SRequired -> RequiredInsert mempty
    SOptional -> OptionalInsert (Just mempty)


type Name :: Context
newtype Name spec = Name String
  deriving newtype (IsString, Monoid, Semigroup)


type Labels :: Context
newtype Labels spec = Labels (NonEmpty String)
  deriving newtype (Semigroup)


type Result :: Context
data Result spec where
  Result :: ()
    => Value nullability (ToType blueprint)
    -> Result ('Spec necessity nullability blueprint)


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , Semigroup (ToType blueprint)
  ) =>
  Semigroup (Result spec)
 where
  Result a <> Result b = Result (a <> b)


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , KnownNullability nullability
  , Monoid (ToType blueprint)
  ) => Monoid (Result spec)
 where
  mempty = Result mempty


type IsSpecialContext :: Context -> Bool
type family IsSpecialContext context where
  IsSpecialContext Aggregation = 'True
  IsSpecialContext DB = 'True
  IsSpecialContext Insert = 'True
  IsSpecialContext Result = 'True
  IsSpecialContext Structure = 'True
  IsSpecialContext _ = 'False
