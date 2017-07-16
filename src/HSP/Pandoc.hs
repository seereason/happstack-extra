{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module HSP.Pandoc where

import Data.Default (def)
import qualified Data.Text.Lazy as TL
import HSP
import Text.Pandoc (Pandoc, writeHtmlString)

instance (XMLGen m, EmbedAsChild m XML) => (EmbedAsChild m Pandoc) where
    asChild pandoc = asChild (cdata $ TL.pack $ writeHtmlString def pandoc)
