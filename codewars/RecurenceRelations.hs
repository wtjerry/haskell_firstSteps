module RecurenceRelations where

import qualified Data.List as L
import Data.Map.Strict as M

-- not performant enough
-- evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
-- evaluateFunction f n =
--   case f n of
--     (Left l) -> l
--     (Right (as, b_func)) -> b_func (L.map (evaluateFunction f) as)


-- more performant thanks to memoizing already calculated results
evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f x = snd $ memoize x M.empty
  where
    memoize n m =
      case M.lookup n m of
        (Just v) -> (m, v)
        Nothing ->
          case f n of
            (Left v) -> (M.insert x v m, v)
            (Right (as, aggF)) ->
              let foldFunc (accM, accBs) a =
                    let (m, v) = memoize a accM
                     in (m, v : accBs)
              in let (m', bs) = L.foldl foldFunc (m, []) as
                  in let v = aggF bs
                      in (M.insert n v m', v)

