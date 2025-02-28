module Rel8.Query.Maybe
  ( optional
  , catMaybeTable
  , traverseMaybeTable
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- rel8
import Rel8.Expr.Bool ( true )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Opaleye ( toPrimExpr, traversePrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Filter ( where_ )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Table.Maybe ( MaybeTable( MaybeTable ), isJustTable )
import Rel8.Table.Tag ( Tag(..), fromExpr )


-- | Convert a query that might return zero rows to a query that always returns
-- at least one row.
--
-- To speak in more concrete terms, 'optional' is most useful to write @LEFT
-- JOIN@s.
optional :: Query a -> Query (MaybeTable a)
optional = mapOpaleye $ \query -> Opaleye.QueryArr $ \i -> case i of
  (_, left, tag) -> (ma', join, tag'')
    where
      (ma, right, tag') = Opaleye.runSimpleQueryArr (pure <$> query) ((), tag)
      MaybeTable Tag {expr = just} a = ma
      (just', bindings) = Opaleye.run $ do
        traversePrimExpr (Opaleye.extractAttr "isJust" tag') just
      tag'' = Opaleye.next tag'
      join = Opaleye.Join Opaleye.LeftJoin on [] bindings left right
        where
          on = toPrimExpr true
      ma' = MaybeTable (fromExpr just') a


-- | Filter out 'MaybeTable's, returning only the tables that are not-null.
--
-- This operation can be used to "undo" the effect of 'optional', which
-- operationally is like turning a @LEFT JOIN@ back into a full @JOIN@.  You
-- can think of this as analogous to 'Data.Maybe.catMaybes'.
catMaybeTable :: MaybeTable a -> Query a
catMaybeTable ma@(MaybeTable _ a) = do
  where_ $ isJustTable ma
  pure a


-- | Extend an optional query with another query.  This is useful if you want
-- to step through multiple @LEFT JOINs@.
--
-- Note that @traverseMaybeTable@ takes a @a -> Query b@ function, which means
-- you also have the ability to "expand" one row into multiple rows.  If the 
-- @a -> Query b@ function returns no rows, then the resulting query will also
-- have no rows. However, regardless of the given @a -> Query b@ function, if
-- the input is @nothingTable@, you will always get exactly one @nothingTable@
-- back.
traverseMaybeTable :: (a -> Query b) -> MaybeTable a -> Query (MaybeTable b)
traverseMaybeTable query ma@(MaybeTable input _) = do
  MaybeTable output b <- optional (query =<< catMaybeTable ma)
  where_ $ expr output ==. expr input
  pure $ MaybeTable input b
