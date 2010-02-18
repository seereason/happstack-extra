module HSP.Applicative where

import Control.Applicative
import Control.Monad.Identity
import HSP.Identity

-- send upstream to applicative-extra
instance Applicative Identity where
    pure = return
    (<*>) = ap
