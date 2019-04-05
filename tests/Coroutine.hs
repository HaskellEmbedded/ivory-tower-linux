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
  monitor "counter" $ do
    s <- state "local_st"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \_ -> do
        s += 1
        emit e (constRef (s :: Ref 'Global ('Stored Uint8)))

  monitor "coroutine" $ do
    monitorModuleDef $ do
      uses_printk

    coroutineHandler systemInit c1out "chan1msg" $ do
      return $ CoroutineBody $ \yield -> do
        call_ printk "Coroutine init"
        forever $ do
          m <- yield
          val <- deref m
          call_ printk_u8 "Hello coroutine! #%u" val

main :: IO ()
main = compileTowerLinux (const $ return ()) app
