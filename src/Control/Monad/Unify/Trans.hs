
module Control.Monad.Unify.Trans where

import Control.Monad.Unify.Class

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)

import Data.Traversable (Traversable(..), traverse)
import Control.Monad (MonadPlus(..))


type Name = String

newtype UnificationVar = MKUnificationVar Name
  deriving (Eq, Ord, Show)

unUnificationVar :: UnificationVar -> Name
unUnificationVar (MKUnificationVar n) = n

instance UnificationVariable UnificationVar where
  variables = toStream (map MKUnificationVar names)
    where
      toStream (x:xs) = Stream x (toStream xs)
      names = [ [c] | c <- ['a'..'z'] ] ++ [ c : show n | n <- [1..], c <- ['a'..'z'] ] 


-- data UnificationState k f 
--   = MkUnifyState { boundVariables :: Map k { equivalence :: Set k, boundTo :: Maybe (f k) },
--     currentVariables :: Stream k
--   }

data UnificationInfo k f
  = MkUnificationInfo 
    { equivalence :: Set k
    , boundTo :: Maybe (f k) }

type UnificationState k f = Map k (UnificationInfo k f) 
