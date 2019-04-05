{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.OS.Linux.Tower (
  compileTowerLinux
) where

import Prelude ()
import Prelude.Compat

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe
import Ivory.Artifact
import Ivory.Compile.C.CmdlineFrontend (runCompiler)
import Ivory.Language
import Ivory.OS.Linux.Tower.KThread
import Ivory.OS.Linux.Tower.Time
import Ivory.Stdlib.Control
import Ivory.Tower
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Backend
import Ivory.Tower.Options
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.SignalCode

import qualified Paths_ivory_tower_linux as P

data MonitorCode = MonitorCode
  { monitorGenCode :: ModuleDef
  , monitorUserCode :: ModuleDef
  }

instance Semigroup MonitorCode where
  (<>) a b = MonitorCode
    { monitorGenCode = monitorGenCode a >> monitorGenCode b
    , monitorUserCode = monitorUserCode a >> monitorUserCode b
    }

instance Monoid MonitorCode where
  mempty = MonitorCode mempty mempty

data EmitterCode = EmitterCode
  { emitterInit :: forall eff. Ivory eff ()
  , emitterDeliver :: forall eff. Ivory eff ()
  , emitterRecipients :: [String]
  , emitterCode :: ModuleDef
  }

data LinuxBackend = LinuxBackend

instance TowerBackend LinuxBackend where
  newtype TowerBackendCallback LinuxBackend a = LinuxCallback (forall s. (Def ('[ConstRef s a] ':-> ()), ModuleDef))
  newtype TowerBackendEmitter LinuxBackend = LinuxEmitter (Maybe EmitterCode)
  data TowerBackendHandler LinuxBackend a = LinuxHandler
    { handlerChan :: AST.Chan
    , handlerRecipients :: [String]
    , handlerProcName :: String
    , handlerProc :: forall s. Def ('[ConstRef s a] ':-> ())
    , handlerCode :: MonitorCode
    }
  newtype TowerBackendMonitor LinuxBackend = LinuxMonitor
    (Dependencies -> Map.Map String Module -> (Map.Map String Module, Map.Map AST.Chan [String], [Module]))
  newtype TowerBackendOutput LinuxBackend = LinuxOutput [TowerBackendMonitor LinuxBackend]

  callbackImpl _ ast f = LinuxCallback $
    let p = proc (showUnique ast) $ \ r -> body $ noReturn $ f r
    in (p, incl p)

  emitterImpl _ _ [] = (Emitter $ const $ return (), LinuxEmitter Nothing)
  emitterImpl _ ast handlers =
    ( Emitter $ call_ $ eproc messageAt
    , LinuxEmitter $ Just EmitterCode
        { emitterInit = store (addrOf messageCount) 0
        , emitterDeliver = do
            mc <- deref (addrOf messageCount)
            forM_ (zip messages [0..]) $ \ (m, index) ->
              when (fromInteger index <? mc) $
                forM_ handlers $ \ h ->
                  call_ (handlerProc h) (constRef (addrOf m))
        , emitterRecipients = map handlerProcName handlers
        , emitterCode = do
            incl $ eproc messageAt
            private $ do
              mapM_ defMemArea messages
              defMemArea messageCount
        }
    )
    where
    max_messages = AST.emitter_bound ast - 1
    messageCount :: MemArea ('Stored Uint32)
    messageCount = area (named "message_count") Nothing

    messages = [ area (named ("message_" ++ show d)) Nothing
               | d <- [0..max_messages] ]

    messageAt idx = foldl aux dflt (zip messages [0..])
      where
      dflt = addrOf (messages !! 0) -- Should be impossible.
      aux basecase (msg, midx) =
        (fromInteger midx ==? idx) ? (addrOf msg, basecase)

    eproc :: IvoryArea b => (Uint32 -> Ref s b) -> Def ('[ConstRef s' b] ':-> ())
    eproc mAt = voidProc (named "emit") $ \ msg -> body $ do
      mc <- deref (addrOf messageCount)
      when (mc <=? fromInteger max_messages) $ do
        store (addrOf messageCount) (mc + 1)
        storedmsg <- assign (mAt mc)
        refCopy storedmsg msg

    named suffix = showUnique (AST.emitter_name ast) ++ "_" ++ suffix

  handlerImpl _ ast emitters callbacks = h
    where
    ems = [ e | LinuxEmitter (Just e) <- emitters ]
    h = LinuxHandler
      { handlerChan = AST.handler_chan ast
      , handlerRecipients = concatMap emitterRecipients ems
      , handlerProcName = "handler_run_" ++ AST.handlerName ast
      , handlerProc = voidProc (handlerProcName h) $ \ msg -> body $ do
          mapM_ emitterInit ems
          forM_ callbacks $ \ (LinuxCallback (cb, _)) -> call_ cb msg
          mapM_ emitterDeliver ems
      , handlerCode = MonitorCode
          { monitorUserCode = forM_ callbacks $ \ (LinuxCallback (_, d)) -> d
          , monitorGenCode = do
              mapM_ emitterCode ems
              incl $ handlerProc h
          }
      }

  monitorImpl _ ast handlers moddef = LinuxMonitor fromModuleMap
    where
    monitorRecipients = concat [ handlerRecipients h | SomeHandler h <- handlers ]
    mods = mconcat [ handlerCode h | SomeHandler h <- handlers ]
    chanMap = Map.fromListWith (++)
      [ (handlerChan h, [handlerProcName h]) | SomeHandler h <- handlers ]

    fromModuleMap deps moduleMap = (thisModuleMap, chanMap, [userMod, genMod])
      where
      thisModuleMap = Map.fromList
        [ (handlerProcName h, genMod) | SomeHandler h <- handlers ]
      otherModuleMap = moduleMap Map.\\ thisModuleMap
      genMod = package ("tower_gen_" ++ AST.monitorName ast) $ do
        depend userMod
        mapM_ depend $ dependencies_depends deps
        mapM_ depend $ mapMaybe (flip Map.lookup otherModuleMap) monitorRecipients
        monitorGenCode mods
      userMod = package ("tower_user_" ++ AST.monitorName ast) $ do
        depend genMod
        mapM_ depend $ dependencies_depends deps
        private moddef
        monitorUserCode mods

  towerImpl _ _ monitors = LinuxOutput monitors



compileTowerLinux :: (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerLinux makeEnv twr = do
  (copts, topts) <- towerGetOpts
  env <- makeEnv topts
  let (_ast, LinuxOutput monitors, deps, sigs) = runTower_ LinuxBackend twr env

  let moduleMap = Map.unions moduleMaps
      (moduleMaps, chanMaps, monitorModules) = unzip3
        [ m deps moduleMap | LinuxMonitor m <- monitors ]

  let chanMap = Map.unionsWith (++) chanMaps

  let callHandlers
        :: (GetAlloc eff ~ 'Scope s1,  GetReturn eff ~ 'Returns TimRestart)
        => [String] -> Maybe (Ref r ('Struct "hrtimer"), Microseconds) -> Ivory eff ()
      callHandlers names mtiming = do
        now <- call ktime_get
        t_ptr <- fmap constRef $ local $ ival $
          fromIMicroseconds now -- (castDefault $ now * 1e6 :: Sint64)
        let itimeStub :: String -> Def ('[ConstRef s ('Stored ITime)] ':-> ())
            itimeStub name = proc name $ const $ body $ return ()
        forM_ names $ \ name -> call_ (itimeStub name) t_ptr

        let dtime t = (castDefault :: IDouble -> Uint64) $ (fromInteger $ toMicroseconds t) * 1.0e3

        case mtiming of
          Nothing -> return () -- hrtimer_NORESTART
          Just (hrtimer, ms) -> do
            ktime <- call ktime_set 0 (dtime ms) -- period in nanoseconds, phase?
            call_ hrtimer_forward hrtimer now ktime
            ret hrtimer_RESTART
        --ret r

  let signalChannels = Map.fromAscList
        [ (AST.signal_name s, xs)
        | (AST.ChanSignal s, xs) <- Map.toAscList chanMap
        ]

  let signalHandlers = Map.elems $ Map.intersectionWith (,) signalChannels $ signalcode_signals sigs

  let periodicHandlers = do
        (AST.ChanPeriod p, names) <- Map.toList chanMap

        let cbname = "elapsed_" ++ prettyTime (AST.period_dt p) ++
              if toMicroseconds (AST.period_phase p) == 0
              then ""
              else "_phase_" ++ prettyTime (AST.period_phase p)

        return (p, proc cbname $ \hrtimer -> body $ callHandlers names (Just (hrtimer, AST.period_dt p)))

  let hrtimers :: [MemArea ('Struct "hrtimer")]
      hrtimers = [area ("hrtimer" ++ (show d)) Nothing
                 | d <- [0..(length periodicHandlers - 1)] ]

  let entryProc = proc "tower_main" $ body $ do

        forM_ (zip periodicHandlers [0..]) $ \((p, cb), i) -> do
          comment "Here we start hrtimers"
          let dtime t = (castDefault :: IDouble -> Uint64) $ (fromInteger $ toMicroseconds t) * 1.0e3
              hrtimer = addrOf (hrtimers !! i)
          ktime <- call ktime_set 0 (dtime $ AST.period_dt p) -- period in nanoseconds, phase?
          call_ hrtimer_init hrtimer clock_MONOTONIC hrtimer_MODE_REL
          call_ hrtimer_set_callback hrtimer (procPtr cb)
          call_ hrtimer_start hrtimer ktime hrtimer_MODE_REL

        comment "Signalcode"
        signalcode_init sigs
        comment "Inits"
        maybe (return ()) (flip callHandlers Nothing) (Map.lookup (AST.ChanInit AST.Init) chanMap)

        --call_ kthread_create nullPtr main_loop "main_loop"
        ret (0 :: Uint32)

  let exitProc = voidProc "tower_exit" $ body $ do
        forM_ (zip periodicHandlers [0..]) $ \((p, cb), i) -> do
          call_ hrtimer_cancel $ addrOf (hrtimers !! i)

  let initModule = package "tower_init" $ do
        mapM_ depend $ Map.elems moduleMap

        uses_kthreads
        uses_timers

        incl entryProc
        incl exitProc
        private $ do
          mapM_ defMemArea hrtimers
          --forM_ periodicHandlers $ \ (p, cb) -> do
          {-
          forM_ signalHandlers $ \ (handlers, code) -> unGeneratedSignal code $ do
            main_loop <- call ev_default_loop 0
            callHandlers main_loop handlers
          -}
          mapM_ (incl . snd) periodicHandlers

  let mods = initModule : concat monitorModules ++ dependencies_modules deps
  let artifacts = (makefile mods : dependencies_artifacts deps) ++ linuxSupport

  runCompiler mods artifacts copts

linuxSupport :: [Located Artifact]
linuxSupport =
  [ Src  $ artifactCabalFile P.getDataDir "support/linux_support.c"
  , Incl $ artifactCabalFile P.getDataDir "support/linux_support.h"
  , Src  $ artifactCabalFile P.getDataDir "support/linux_module.c"
  ]

makefile :: [Module] -> Located Artifact
makefile modules = Root $ artifactString "Makefile" $ unlines
  [ "PWD ?= $(shell pwd)"
  , "obj-m := tower-linux.o"
  , "tower-linux-y := linux_module.o linux_support.o " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ]
--  , moduleName (head modules) ++ ": $(OBJS)"
  , "default:"
  , "\tmake -C $(shell nix-build -E '(import <nixpkgs> {}).linux_latest.dev' --no-out-link)/lib/modules/*/build M=$(PWD) modules"
  , "load:"
  , "\tsudo insmod tower-linux.ko"
  , "rmmod:"
  , "\tsudo rmmod tower-linux"
  , "clean:"
  , "\t-rm -f *.o *.ko Module.symvers modules.order *.cmd .*.cmd .cache.mk *.mod.c .tmp_versions"
  , ".PHONY: clean"
  ]
