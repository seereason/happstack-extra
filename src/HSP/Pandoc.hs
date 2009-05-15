{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module HSP.Pandoc where

import HSP
import Text.Pandoc (Pandoc, defaultWriterOptions, writeHtmlString)

instance (EmbedAsChild m XML) => (EmbedAsChild m Pandoc) where
    asChild pandoc = asChild (cdata $ writeHtmlString defaultWriterOptions pandoc)
