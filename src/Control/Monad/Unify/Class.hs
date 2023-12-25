{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Unify.Class where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)

import Data.Traversable (Traversable(..), traverse)
import Control.Monad (MonadPlus(..))

-- type alias UnificationInfo =
--     { equivalence : Set String
--     , term : Maybe (Term UnificationVar)
--     }
-- type alias UnificationState = Dict String UnificationInfo
-- type UnificationState k f =
--     { boundVariables :: Map k { equivalence :: Set k, boundTo :: Maybe (f k) }
--     , currentVariables :: Stream k
--     }
-- newtype UnifyT k f m a = MkUnifyT (StateT (UnificationState k f) m a)


data Stream a = Stream a (Stream a)

pop :: Stream a -> (a, Stream a)
pop (Stream a as) = (a, as)

toList :: Stream a -> [a]
toList (Stream a as) = a : toList as

instance Show a => Show (Stream a) where
  show = show . take 5 . toList

class Ord k => UnificationVariable k where
  variables :: Stream k

data ComparisonAction k f
  = Merge k k
  | Store k (f k)
  | Fail

class (UnificationVariable k, Traversable f) => 
  Unifiable k f where
    variable :: k -> f k
    squash :: f (f k) -> f k
    alongside :: f k -> f k -> f (ComparisonAction k f)



class (Unifiable k f, MonadPlus m) => 
  MonadUnify k f m | m -> k, m -> f where
    fresh :: m k
    merge :: k -> k -> m ()
    store :: k -> f k -> m ()
    reify :: f k -> m (f k)

process :: MonadUnify k f m => ComparisonAction k f -> m ()
process (Merge k1 k2) = merge k1 k2
process (Store k t) = store k t
process Fail = mzero

unify :: MonadUnify k f m => f k -> f k -> m ()
unify t1 t2 = do
  _ <- traverse process (alongside t1 t2)
  pure () 


instance MonadUnify k f m => MonadUnify k f (StateT s m) where
    fresh = lift fresh
    merge m k = lift $ merge m k
    store k f = lift $ store k f
    reify f = lift $ reify f
