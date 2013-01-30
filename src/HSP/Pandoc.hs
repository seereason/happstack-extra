{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module HSP.Pandoc where

import Data.Default (def)
import HSP
import Text.Pandoc (Pandoc, writeHtmlString)

instance (EmbedAsChild m XML) => (EmbedAsChild m Pandoc) where
    asChild pandoc = asChild (cdata $ writeHtmlString def pandoc)
