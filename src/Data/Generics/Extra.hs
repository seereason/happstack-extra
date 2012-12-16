module Data.Generics.Extra (gFind, gFind') where

import Control.Monad (MonadPlus, msum)
import Data.Generics (Data, Typeable, listify)
import Data.Maybe (fromJust)

-- | @gFind a@ will extract any elements of type @b@ from
-- @a@'s structure in accordance with the MonadPlus
-- instance, e.g. Maybe Foo will return the first Foo
-- found while [Foo] will return the list of Foos found.
gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)

-- | Acts as gFind but will throw an exception if
-- nothing is found.
gFind' :: (Data a, Typeable b) => a -> b
gFind' = fromJust . gFind
--Monad versions
