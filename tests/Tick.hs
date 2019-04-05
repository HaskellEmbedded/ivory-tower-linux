{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Ivory.Tower
import Ivory.Stdlib
import Ivory.Language
import Ivory.OS.Linux.Tower
import Ivory.OS.Linux.Tower.IO

app :: Tower e ()
app = do
  (c1in, c1out) <- channel
  per <- period (Milliseconds 1000)
  monitor "m1" $ do
    s <- state "local_st"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \_ -> do
        s += 1
        emit e (constRef (s :: Ref 'Global ('Stored Uint8)))

  monitor "m2" $ do
    monitorModuleDef $ do
      uses_printk

    s <- state "last_m2_chan1_message"
    handler c1out "chan1msg" $ do
      callback $ \m -> do
        refCopy s m
        val <- deref m
        call_ printk_u8 "Hello Linux! #%u" val

main :: IO ()
main = compileTowerLinux (const $ return ()) app
