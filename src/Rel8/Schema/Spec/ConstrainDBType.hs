{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec.ConstrainDBType
  ( ConstrainDBType
  , dbTypeNullity, dbTypeDict
  , nullifier, unnullifier
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Null
  ( Nullify, Unnullify
  , Nullity( Null, NotNull )
  , Sql, nullable
  )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec( SSpec, nullity ) )


type ConstrainDBType :: (Type -> Constraint) -> Spec -> Constraint
class
  ( forall c a. ()
     => (spec ~ 'Spec a)
     => (forall x. (constraint x => c x)) => Sql c a
  )
  => ConstrainDBType constraint spec
instance
  ( spec ~ 'Spec a
  , Sql constraint a
  )
  => ConstrainDBType constraint spec


dbTypeNullity :: Dict (ConstrainDBType c) ('Spec a) -> Nullity a
dbTypeNullity = step2 . step1
  where
    step1 :: Dict (ConstrainDBType c) ('Spec a) -> Dict (Sql c) a
    step1 Dict = Dict

    step2 :: Dict (Sql c) a -> Nullity a
    step2 Dict = nullable


dbTypeDict :: Dict (ConstrainDBType c) ('Spec a) -> Dict c (Unnullify a)
dbTypeDict = step2 . step1
  where
    step1 :: Dict (ConstrainDBType c) ('Spec a) -> Dict (Sql c) a
    step1 Dict = Dict

    step2 :: Dict (Sql c) a -> Dict c (Unnullify a)
    step2 Dict = Dict


fromNullityDict :: Nullity a -> Dict c (Unnullify a) -> Dict (ConstrainDBType c) ('Spec a)
fromNullityDict Null Dict = Dict
fromNullityDict NotNull Dict = Dict


nullifier :: ()
  => SSpec ('Spec a)
  -> Dict (ConstrainDBType c) ('Spec a)
  -> Dict (ConstrainDBType c) ('Spec (Nullify a))
nullifier SSpec {} dict = case dbTypeDict dict of
  Dict -> case dbTypeNullity dict of
    Null -> Dict
    NotNull -> Dict


unnullifier :: ()
  => SSpec ('Spec a)
  -> Dict (ConstrainDBType c) ('Spec (Nullify a))
  -> Dict (ConstrainDBType c) ('Spec a)
unnullifier SSpec {nullity} dict = case dbTypeDict dict of
  Dict -> case nullity of
    Null -> Dict
    NotNull -> case dbTypeNullity dict of
      Null -> fromNullityDict nullity Dict
