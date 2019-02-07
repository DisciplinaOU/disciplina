{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Implementation of response length counting.
module Dscp.Educator.Web.Util.Count
    ( counting_
    , countPaginate_
    , runCountedSelect
    , runCountedSelectMap
    ) where

import Database.Beam.Backend (FromBackendRow)
import Database.Beam.Backend.SQL (Sql92SelectSelectTableSyntax, Sql92SelectTableExpressionSyntax)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgSelectSyntax)
import Database.Beam.Query (Q, QGenExpr, SqlSelect, just_, nothing_)
import Database.Beam.Query.Internal (ProjectibleWithPredicate, QNested, QValueContext, ValueContext)
import Database.Beam.Schema (C, Nullable)
import Servant.Util (PaginationSpec)
import Servant.Util.Beam.Postgres (paginate_)

import Dscp.DB.SQL
import Dscp.Educator.Web.Educator.Types


-- | Helper Beam-style type which keeps either response length or response itself.
type CountedT a f = (C (Nullable f) Int, C (Nullable f) a)

-- | Modify a query so that, when 'True' is passed, only response length is returned.
counting_
    :: ( f ~ QGenExpr QValueContext exprSyntax
       , exprSyntax ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
       , _
       )
    => Bool
    -> (forall s0. ProjectibleWithPredicate ValueContext exprSyntax (f (QNested s0) a) =>
                   Q select db s0 (f s0 a))
    -> (Q select db s (CountedT a (f s)))
counting_ countOnly query
    | countOnly = do
        count <- just_ <$> aggregate_ (\_ -> countAll_) query
        return (count, nothing_)
    | otherwise = query <&> \q -> (nothing_, just_ q)

-- | Like 'counting_', disables pagination when flag is 'True'.
countPaginate_
    :: ( f ~ QGenExpr QValueContext exprSyntax
       , exprSyntax ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
       , _)
    => Bool
    -> PaginationSpec
    -> (forall s0. Q select db s0 (f s0 a))
    -> (Q select db s (CountedT a (f s)))
countPaginate_ countOnly pagination
    | countOnly = counting_ countOnly
    | otherwise = \query -> counting_ countOnly $ paginate_ pagination query

-- | Interpret a counted response returned by 'runSelect'.
fromBeamCounted :: [CountedT a Identity] -> Counted a
fromBeamCounted = \case
    [] -> Counted{ cCount = 0, cItems = Just [] }  -- TODO: is it ok to show both diregard "onlyCount" value?
    ((Just c, _) : _) -> Counted{ cCount = c, cItems = Nothing }
    cs -> Counted
          { cCount = length cs
          , cItems = Just $ map (fromMaybe (error ":shrug:") . snd) cs
          }

-- | Counting version of 'runSelectMap', couple it with 'counting_' or similar functions.
runCountedSelectMap
    :: (MonadIO m, FromBackendRow Postgres a)
    => (a -> b)
    -> SqlSelect PgSelectSyntax (CountedT a Identity)
    -> DBT t m (Counted b)
runCountedSelectMap f = fmap (fmap f . fromBeamCounted) . runSelect

-- | Counting version of 'runSelect', couple it with 'counting_' or similar functions.
runCountedSelect
    :: (MonadIO m, FromBackendRow Postgres a)
    => SqlSelect PgSelectSyntax (CountedT a Identity)
    -> DBT t m (Counted a)
runCountedSelect = runCountedSelectMap id
