module GHC.TypeLits.Presburger.Syntax
  ( module GHC.TypeLits.Presburger.Syntax.Base
  , toSATExp, toSATProp
  , signConditions, erasePropVars
  ) where
import qualified Data.Integer.SAT                    as SAT
import           GHC.TypeLits.Presburger.Syntax.Base

import Control.Monad
import Control.Monad.Trans.RWS (get, modify, runRWS, tell)
import Data.Void

import qualified Data.Set as Set

toSATExp :: SimpleExp Int -> SAT.Expr
toSATExp (K n)      = SAT.K $ fromIntegral n
toSATExp (EVar v)   = SAT.Var $ SAT.toName v
toSATExp (l :+ r)   = toSATExp l SAT.:+ toSATExp r
toSATExp (l :- r)   = toSATExp l SAT.:- toSATExp r
toSATExp (i :* r)   = fromIntegral i SAT.:* toSATExp r
toSATExp (Negate e) = SAT.Negate $ toSATExp e

erasePropVars :: SimpleProp u v -> Maybe (SimpleProp u Void)
erasePropVars (PVar _)   = Nothing
erasePropVars SPTrue     = return SPTrue
erasePropVars SPFalse    = return SPTrue
erasePropVars (l :== r)  = return (l :== r)
erasePropVars (l :/= r)  = return (l :/= r)
erasePropVars (l :<= r)  = return (l :<= r)
erasePropVars (l :< r)   = return (l :< r)
erasePropVars (Not e)    = Not <$> erasePropVars e
erasePropVars (l :&& r)  = (:&&) <$> erasePropVars l <*> erasePropVars r
erasePropVars (l :|| r)  = (:||) <$> erasePropVars l <*> erasePropVars r
erasePropVars (l :==> r) = (:==>) <$> erasePropVars l <*> erasePropVars r
erasePropVars (l :<=> r) = (:<=>) <$> erasePropVars l <*> erasePropVars r

toSATProp :: SimpleProp Int Void -> SAT.Prop
toSATProp (PVar v)   = absurd v
toSATProp SPTrue     = SAT.PTrue
toSATProp SPFalse    = SAT.PFalse
toSATProp (l :== r)  = toSATExp l SAT.:== toSATExp r
toSATProp (l :/= r)  = toSATExp l SAT.:/= toSATExp r
toSATProp (l :<= r)  = toSATExp l SAT.:<= toSATExp r
toSATProp (l :<  r)  = toSATExp l SAT.:< toSATExp r
toSATProp (Not e)    = SAT.Not $ toSATProp e
toSATProp (l :|| r)  = toSATProp l SAT.:|| toSATProp r
toSATProp (l :&& r)  = toSATProp l SAT.:&& toSATProp r
toSATProp (l :==> r) = SAT.Not (toSATProp l) SAT.:|| toSATProp r
toSATProp (l :<=> r) =
  let (l', r') = (toSATProp l, toSATProp r)
  in (l' SAT.:&& r') SAT.:|| (SAT.Not l' SAT.:&& SAT.Not r')

signConditions :: (Ord u) => SimpleProp u Void -> Set.Set (SimpleProp u Void)
signConditions p0 =
  let (_, _, w) = runRWS (loop p0) () Set.empty
  in w
  where
    loop (PVar v)   = absurd v
    loop SPTrue     = return ()
    loop SPFalse    = return ()
    loop (q :|| r)  = loop q >> loop r
    loop (q :&& r)  = loop q >> loop r
    loop (q :==> r) = loop q >> loop r
    loop (q :<=> r) = loop q >> loop r
    loop (Not q)    = loop q
    loop (l :<= r)  = loopExp l >> loopExp r
    loop (l :< r)   = loopExp l >> loopExp r
    loop (l :== r)  = loopExp l >> loopExp r
    loop (l :/= r)  = loopExp l >> loopExp r

    assertPositive pos = do
      dic <- get
      unless (Set.member pos dic) $ do
        modify $ Set.insert pos
        tell $ Set.fromList [K 0 :<= pos]

    loopExp e@(Negate _) = loopExp e >> assertPositive e
    loopExp e@(l :- r)   = do
      loopExp l >> loopExp r
      assertPositive e
    loopExp (l :+ r) = loopExp l >> loopExp r
    loopExp e@EVar{} = assertPositive e
    loopExp (_ :* e) = loopExp e
    loopExp (K _)    = return ()
