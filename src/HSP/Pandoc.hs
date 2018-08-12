{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module HSP.Pandoc where

import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import HSP
import Text.Pandoc (Pandoc, runPure, writeHtml5String)

instance (XMLGen m, EmbedAsChild m XML) => (EmbedAsChild m Pandoc) where
    asChild pandoc = asChild (cdata $ either (TL.pack . show) TL.fromStrict $ runPure $ writeHtml5String def pandoc)
