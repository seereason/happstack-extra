{-# OPTIONS_GHC -fno-warn-orphans -F -pgmF trhsx #-}
-- |simple example of using Identity XML monad
module Main where

import HSP
import HSP.Identity

page :: XML
page = evalIdentity $
       <html>
        <head>
         <title>Literal XML in a pure function</title>
        </head>
        <body>
         <p>Purely for demo purposes.</p>
        </body>
       </html>

main :: IO ()
main = putStrLn (renderAsHTML page)