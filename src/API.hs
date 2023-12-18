-- |
-- Module: API
--
-- An API around the 'Rules' datatype, so that authors of
-- rules do not have to manually generate 'RuleId's and
-- 'ActionId's.
module API
  ( -- * Rules API
    registerRule, registerAction

  -- ** Local name generation (for t'ActionId')
  , FreshT, hoist

  -- ** Internal functions
  , runFreshT, computeRules
  )
  where

-- base
import Data.Functor.Identity
  ( Identity(..) )

-- containers
import qualified Data.Map.Strict as Map
  ( insert, lookupMax )

-- transformers
import Control.Monad.Trans.State.Strict
  ( get, put )
import Control.Monad.Trans.Writer.CPS
  ( WriterT, mapWriterT, tell )

-- john-rules
import Rules

--------------------------------------------------------------------------------
-- API functions

hoist :: ( Monad m, Monoid w )
      => ( forall u. n u -> m u )
      -> WriterT w n a -> WriterT w m a
hoist h = mapWriterT h

-- | SetupHooks internal function used to implement 'registerAction'.
register :: ( Monad m, Ord x_id )
         => x_id -> ( x_id -> x_id )
         -> x -> FreshT x x_id m x_id
register zeroId succId rule = FreshT $ do
  oldRules <- get
  let newId
        | Just ( x_id, _ ) <- Map.lookupMax oldRules
        = succId x_id
        | otherwise
        = zeroId
      !newRules = Map.insert newId rule oldRules
  put newRules
  return newId

-- | Register a rule.
registerRule :: Monad m => Rule -> RulesT m ()
registerRule r = tell $ [r]

-- | Register an action. Returns a unique identifier for that action.
registerAction :: Action -> FreshT Action ActionId Identity ActionId
registerAction = register ( ActionId 1 ) ( \ ( ActionId i ) -> ActionId ( i + 1 ) )
