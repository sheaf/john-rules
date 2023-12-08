-- |
-- Module: API
--
-- An API around the 'Rules' datatype, so that authors of
-- rules do not have to manually generate 'RuleId's and
-- 'ActionId's.
module API
  ( -- * Rules API
    RulesM(..), fromRulesM
  , ActionsM, RulesT
  , registerRule, registerAction

  -- ** Local name generation (for t'RuleId' and t'ActionId')
  , FreshT, hoist

  -- ** Internal functions
  , runFreshT, computeRules
  )
  where

-- base
import Control.Monad.Fix
  ( MonadFix )
import Data.Functor.Identity
  ( Identity(..) )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( empty, insert, lookupMax )

-- transformers
import Control.Monad.Trans.Class
  ( MonadTrans(..) )
import Control.Monad.Trans.State.Strict
  ( StateT(..), get, put )

-- john-rules
import Rules

--------------------------------------------------------------------------------

-- | A utility type that supports registration of 'Action's and 'Rules'
--
-- Actions are registered using 'registerAction', and rules are registered
-- using 'registerRule'.
--
-- As described in t'Rules', even though 'Rule's and t'Action's are registered
-- separately, they must jointly satisfy the following preconditions
-- (see t'Rule'):
--
--  - the action called by a rule should expect as many arguments as
--    the rule declares dependencies;
--  - the action called by a rule should output the results described by
--    the rule.
newtype RulesM inputs outputs
  = RulesM { runRulesM :: inputs -> ActionsM ( RulesT SmallIO outputs ) }
    -- ^ Return a collection of t'Action's and their t'ActionId's, and then,
    -- in the inner computation, a collection of t'RuleId's, with each t'RuleId'
    -- associated with a corresponding 'Rule' which specifies its dependencies
    -- as well as the t'ActionId' of the t'Action' to run in order to execute
    -- the rule.
    --
    -- You can think of this type signature as:
    --
    -- > inputs -> ( Map ActionId Action, IO ( outputs, Map RuleId Rule ) )
    --
    -- except that it is structured in such a way as to avoid having
    -- to manually create t'ActionId' and t'RuleId' values.

instance Functor ( RulesM inputs ) where
  fmap f ( RulesM r ) = RulesM \ inputs -> fmap ( fmap f ) ( r inputs )
instance Applicative ( RulesM inputs ) where
  pure o = RulesM \ _ -> pure ( pure o )
  ( RulesM f ) <*> ( RulesM a ) =
    RulesM \ inputs ->
      liftA2 (<*>) ( f inputs ) ( a inputs )
-- NB: no Monad instance.

-- | Warning: this 'Semigroup' instance is not commutative.
instance Semigroup outputs => Semigroup ( RulesM inputs outputs ) where
  ( RulesM rs1 ) <> ( RulesM rs2 ) =
    RulesM \ inputs -> do
      x1 <- rs1 inputs
      x2 <- rs2 inputs
      return $ do
         y1 <- x1
         y2 <- x2
         return $ y1 <> y2

instance Monoid outputs => Monoid ( RulesM inputs outputs ) where
  mempty = RulesM \ _ -> pure $ pure mempty

-- | A monad transformer for the registration of values, through the creation
-- of fresh identifiers for these values.
newtype FreshT x x_id m a = FreshT { freshT :: StateT ( Map x_id x ) m a }
  deriving newtype ( Functor, Applicative, Monad, MonadTrans, MonadFix )
runFreshT :: FreshT x x_id m a -> m ( a, Map x_id x )
runFreshT = ( `runStateT` Map.empty ) . freshT

type RulesT = FreshT Rule RuleId
type ActionsM = FreshT Action ActionId Identity

fromRulesM :: RulesM inputs outputs -> Rules inputs outputs
fromRulesM m =
  Rules
    { rules   = \ inputs -> snd $ computeRules inputs m
    , actions = \ inputs -> fst $ computeRules inputs m
    }

computeRules
  :: inputs
  -> RulesM inputs outputs
  -> ( Map ActionId Action, SmallIO ( outputs, Map RuleId Rule ) )
computeRules inputs ( RulesM rs ) =
  ( actionFromId, runFreshT rulesT )
  where
    ( rulesT, actionFromId ) = runIdentity $ runFreshT $ rs inputs

--------------------------------------------------------------------------------
-- API functions

hoist :: ( forall u. n u -> m u )
      -> FreshT x x_id n a -> FreshT x x_id m a
hoist h ( FreshT ( StateT f ) ) = FreshT $ StateT $ \ s -> h $ f s

register :: ( Monad m, Ord x_id )
         => x_id -> ( x_id -> x_id )
         -> x -> FreshT x x_id m x_id
register zeroId succId rule = FreshT do
  oldRules <- get
  let newId
        | Just ( x_id, _ ) <- Map.lookupMax oldRules
        = succId x_id
        | otherwise
        = zeroId
      !newRules = Map.insert newId rule oldRules
  put newRules
  return newId

-- | Register a rule. Returns a unique identifier for that rule.
registerRule :: Monad m => Rule -> FreshT Rule RuleId m RuleId
registerRule = register ( RuleId 1 ) ( \ ( RuleId i ) -> RuleId ( i + 1 ) )

-- | Register an action. Returns a unique identifier for that action.
registerAction :: Monad m => Action -> FreshT Action ActionId m ActionId
registerAction = register ( ActionId 1 ) ( \ ( ActionId i ) -> ActionId ( i + 1 ) )

{-# INLINABLE registerRule #-}
{-# INLINABLE registerAction #-}
{-# SPECIALISE
      registerRule :: Rule -> FreshT Rule RuleId SmallIO RuleId
  #-}
{-# SPECIALISE
      registerAction :: Action -> FreshT Action ActionId Identity ActionId
  #-}
