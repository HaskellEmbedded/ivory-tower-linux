{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.OS.Linux.Tower.Time where

import Ivory.Language
import Ivory.Tower

[ivory|
abstract struct hrtimer "linux/hrtimer.h"
struct timespec64
  { tv_sec :: Stored Sint64
  ; tv_nsec :: Stored Uint64 }
|]

hr_header = "linux/hrtimer.h"
ktime_header = "linux/ktime.h"
timekeeping_header = "linux/timekeeping.h"

uapi_time_header = "uapi/linux/time.h"

type TimMode = Uint32
type TimBase = Uint32
type TimRestart = Uint32

type KTime = Sint64

externB :: String -> TimBase
externB = flip extern hr_header

hrtimer_BASE_MONOTONIC      = externB "HRTIMER_BASE_MONOTONIC"
hrtimer_BASE_REALTIME       = externB "HRTIMER_BASE_REALTIME"
hrtimer_BASE_BOOTTIME       = externB "HRTIMER_BASE_BOOTTIME"
hrtimer_BASE_TAI            = externB "HRTIMER_BASE_TAI"
hrtimer_BASE_MONOTONIC_SOFT = externB "HRTIMER_BASE_MONOTONIC_SOFT"
hrtimer_BASE_REALTIME_SOFT  = externB "HRTIMER_BASE_REALTIME_SOFT"
hrtimer_BASE_BOOTTIME_SOFT  = externB "HRTIMER_BASE_BOOTTIME_SOFT"
hrtimer_BASE_TAI_SOFT       = externB "HRTIMER_BASE_TAI_SOFT"
hrtimer_MAX_CLOCK_BASES     = externB "HRTIMER_MAX_CLOCK_BASES"

externR :: String -> TimRestart
externR = flip extern hr_header

hrtimer_NORESTART           = externR "HRTIMER_NORESTART"
hrtimer_RESTART             = externR "HRTIMER_RESTART"

externM :: String -> TimMode
externM = flip extern hr_header

hrtimer_MODE_ABS             = externM "HRTIMER_MODE_ABS"
hrtimer_MODE_REL             = externM "HRTIMER_MODE_REL"

hrtimer_init :: Def ('[Ref s ('Struct "hrtimer"), ClockID, TimMode] ':-> ())
hrtimer_init = importProc "hrtimer_init" hr_header

hrtimer_start :: Def ('[Ref s ('Struct "hrtimer"), KTime, TimMode] ':-> ())
hrtimer_start = importProc "hrtimer_start" hr_header

hrtimer_cancel :: Def ('[Ref s ('Struct "hrtimer")] ':-> Sint64)
hrtimer_cancel = importProc "hrtimer_cancel" hr_header

hrtimer_forward :: Def ('[Ref s ('Struct "hrtimer"), KTime, KTime] ':-> Uint64)
hrtimer_forward = importProc "hrtimer_forward" hr_header

hrtimer_nanosleep :: Def ('[Ref s ('Struct "timespec64"), TimMode, ClockID] ':-> Uint64)
hrtimer_nanosleep = importProc "hrtimer_nanosleep" hr_header


type TimCallbackPtr s = ProcPtr ('[Ref s ('Struct "hrtimer")] ':-> TimRestart)

hrtimer_set_callback :: Def('[Ref s1 ('Struct "hrtimer"), TimCallbackPtr s2] ':-> ())
hrtimer_set_callback = importProc "hrtimer_set_callback" "linux_support.h"

ktime_set :: Def ('[Sint64, Uint64] ':-> KTime)
ktime_set = importProc "ktime_set" ktime_header

ktime_get :: Def ('[] ':-> KTime)
ktime_get = importProc "ktime_get" timekeeping_header

type ClockID = Uint32

clock_MONOTONIC :: ClockID
clock_MONOTONIC = extern "CLOCK_MONOTONIC" uapi_time_header

uses_timers = do
  inclSym clock_MONOTONIC
  inclSym hrtimer_BASE_MONOTONIC
  inclSym hrtimer_BASE_REALTIME
  inclSym hrtimer_BASE_BOOTTIME
  inclSym hrtimer_BASE_TAI
  inclSym hrtimer_BASE_MONOTONIC_SOFT
  inclSym hrtimer_BASE_REALTIME_SOFT
  inclSym hrtimer_BASE_BOOTTIME_SOFT
  inclSym hrtimer_BASE_TAI_SOFT
  inclSym hrtimer_MAX_CLOCK_BASES
  inclSym hrtimer_MODE_ABS
  inclSym hrtimer_MODE_REL
  inclSym hrtimer_RESTART
  inclSym hrtimer_NORESTART
  inclSym clock_MONOTONIC
  incl hrtimer_init
  incl hrtimer_start
  incl hrtimer_cancel
  incl hrtimer_forward
  incl hrtimer_set_callback
  incl ktime_set
  incl ktime_get
