module Query where

import Control.Monad.Trans.State.Strict (StateT (StateT))

type Result s e a = StateT s (Either e) a
type Query s e a b = a -> Result s e b

success :: a -> Result s e a
success = StateT . (Right .) . (,)

failure :: e -> Result s e a
failure = StateT . const . Left
