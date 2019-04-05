{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.OS.Linux.Tower.KThread where

import Ivory.Language
import Ivory.Language.Pointer
import Ivory.Tower

[ivory|
abstract struct task_struct "linux/sched.h"

struct task_data
  { notmuch :: Stored Uint8 }
|]

type CallbackPtr s = ProcPtr ('[Ref s ('Struct "task_data")] ':-> Sint32)

kthread_create :: Def ('[CallbackPtr s1, Ref s1 ('Struct "task_data"), IString] ':-> Ref s4 ('Struct "task_struct"))
kthread_create = importProc "kthread_create" "linux/kthread.h"

kthread_stop :: Def ('[Ref s ('Struct "task_struct")] ':-> Sint32)
kthread_stop = importProc "kthread_stop" "linux/kthread.h"

wake_up_process :: Def ('[Ref s ('Struct "task_struct")] ':-> Sint32)
wake_up_process = importProc "wake_up_process" "linux/sched.h"

uses_kthreads :: ModuleDef
uses_kthreads = do
  incl kthread_create
  incl kthread_stop
  incl wake_up_process
  defStruct (Proxy :: Proxy "task_data")


