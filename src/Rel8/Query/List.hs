{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}

module Rel8.Query.List
  ( many, some
  , manyExpr, someExpr
  , catListTable, catNonEmptyTable
  , catList, catNonEmpty
  )
where

-- base
import Data.Functor.Identity ( runIdentity )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Col( E, unE ), Expr )
import Rel8.Expr.Aggregate ( listAggExpr, nonEmptyAggExpr )
import Rel8.Expr.Opaleye ( mapPrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Aggregate ( aggregate )
import Rel8.Query.Evaluate ( rebind )
import Rel8.Query.Maybe ( optional )
import Rel8.Schema.HTable.Vectorize ( hunvectorize )
import Rel8.Schema.Null ( Sql, Unnullify )
import Rel8.Schema.Spec ( SSpec( SSpec, info ) )
import Rel8.Table ( Table, fromColumns, toColumns )
import Rel8.Table.Aggregate ( listAgg, nonEmptyAgg )
import Rel8.Table.List ( ListTable( ListTable ) )
import Rel8.Table.Maybe ( maybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable( NonEmptyTable ) )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Array ( extractArrayElement )
import Rel8.Type.Information ( TypeInformation )


-- | Aggregate a 'Query' into a 'ListTable'. If the supplied query returns 0
-- rows, this function will produce a 'Query' that returns one row containing
-- the empty @ListTable@. If the supplied @Query@ does return rows, @many@ will
-- return exactly one row, with a @ListTable@ collecting all returned rows.
--
-- @many@ is analogous to 'Control.Applicative.many' from
-- @Control.Applicative@.
many :: Table Expr a => Query a -> Query (ListTable a)
many =
  fmap (maybeTable mempty (\(ListTable a) -> ListTable a)) .
  optional .
  aggregate .
  fmap (listAgg . toColumns)


-- | Aggregate a 'Query' into a 'NonEmptyTable'. If the supplied query returns
-- 0 rows, this function will produce a 'Query' that is empty - that is, will
-- produce zero @NonEmptyTable@s. If the supplied @Query@ does return rows,
-- @some@ will return exactly one row, with a @NonEmptyTable@ collecting all
-- returned rows.
--
-- @some@ is analogous to 'Control.Applicative.some' from
-- @Control.Applicative@.
some :: Table Expr a => Query a -> Query (NonEmptyTable a)
some =
  fmap (\(NonEmptyTable a) -> NonEmptyTable a) .
  aggregate .
  fmap (nonEmptyAgg . toColumns)


-- | A version of 'many' specialised to single expressions.
manyExpr :: Sql DBType a => Query (Expr a) -> Query (Expr [a])
manyExpr = fmap (maybeTable mempty id) . optional . aggregate . fmap listAggExpr


-- | A version of 'many' specialised to single expressions.
someExpr :: Sql DBType a => Query (Expr a) -> Query (Expr (NonEmpty a))
someExpr = aggregate . fmap nonEmptyAggExpr


-- | Expand a 'ListTable' into a 'Query', where each row in the query is an
-- element of the given @ListTable@.
--
-- @catListTable@ is an inverse to 'many'.
catListTable :: Table Expr a => ListTable a -> Query a
catListTable (ListTable as) = rebind $ fromColumns $ runIdentity $
  hunvectorize (\SSpec {info} -> pure . E . sunnest info . unE) as


-- | Expand a 'NonEmptyTable' into a 'Query', where each row in the query is an
-- element of the given @NonEmptyTable@.
--
-- @catNonEmptyTable@ is an inverse to 'some'.
catNonEmptyTable :: Table Expr a => NonEmptyTable a -> Query a
catNonEmptyTable (NonEmptyTable as) = rebind $ fromColumns $ runIdentity $
  hunvectorize (\SSpec {info} -> pure . E . sunnest info . unE) as


-- | Expand an expression that contains a list into a 'Query', where each row
-- in the query is an element of the given list.
--
-- @catList@ is an inverse to 'manyExpr'.
catList :: Sql DBType a => Expr [a] -> Query (Expr a)
catList = rebind . sunnest typeInformation


-- | Expand an expression that contains a non-empty list into a 'Query', where
-- each row in the query is an element of the given list.
--
-- @catNonEmpty@ is an inverse to 'someExpr'.
catNonEmpty :: Sql DBType a => Expr (NonEmpty a) -> Query (Expr a)
catNonEmpty = rebind . sunnest typeInformation


sunnest :: TypeInformation (Unnullify a) -> Expr (list a) -> Expr a
sunnest info = mapPrimExpr $
  extractArrayElement info .
  Opaleye.UnExpr (Opaleye.UnOpOther "UNNEST")
