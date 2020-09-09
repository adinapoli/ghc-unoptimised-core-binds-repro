
module ToyPlugin where

import qualified Outputable as O
import HscMain
import TcRnMonad
import GHC
import Control.Monad
import CoreMonad
import DynFlags
import HscTypes
import GHC.Paths (libdir)
import Plugins


plugin :: Plugin
plugin = defaultPlugin { pluginRecompile = \_ -> pure ForceRecompile
                       , typeCheckResultAction = typecheckHook
                       , installCoreToDos   = coreHook
                       }

-- | Set the optimization level to 0 (\"-O0\") and clean any plugin to avoid that calling 'desugarModule'
-- might accidentally loop everything.
cleanSummary :: ModSummary -> ModSummary
cleanSummary modSummary = modSummary
  { ms_hspp_opts = unoptFlags (ms_hspp_opts modSummary) { cachedPlugins = [], staticPlugins = [] }
  }

unoptFlags :: DynFlags -> DynFlags
unoptFlags = updOptLevel 0

cleanEnv :: HscEnv -> HscEnv
cleanEnv env = env { hsc_dflags = unoptFlags (hsc_dflags env) }

typecheckHook :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckHook _ modSummary gblEnv = do
  -- Try to produce core binds with optimisations turned off by reusing the 'TcGblEnv'
  -- we got as input.
  let modSum' = cleanSummary modSummary
  env <- cleanEnv <$> getTopEnv
  coreBinds <- runGhcT (Just libdir) $ do
    mg_binds <$> liftIO (hscDesugar env modSum' gblEnv)
  liftIO $ do
    putStrLn $ (replicate 80 '*')
    putStrLn "Core Binds generated with -O0:"
    putStrLn $ O.showSDocUnsafe (O.vcat $ map O.ppr coreBinds)
    putStrLn $ (replicate 80 '*')
  pure gblEnv


coreHook :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
coreHook _ passes = pure (CoreDoPluginPass "ToyPluginPass" myPass : passes)
  where
    myPass :: ModGuts -> CoreM ModGuts
    myPass guts = do
      oLvl <- getOptLevel
      liftIO $
        when (moduleNameString (moduleName . mg_module $ guts) == "Main") $ do
          putStrLn $ (replicate 80 '*')
          putStrLn $ "User optimization level: " <> show oLvl
          putStrLn "Core Binds at the Core phase:"
          putStrLn $ (O.showSDocUnsafe $ O.vcat $ map O.ppr (mg_binds guts))
          putStrLn $ (replicate 80 '*')
      pure guts
    getOptLevel :: (Functor m, HasDynFlags m) => m Int
    getOptLevel = optLevel <$> getDynFlags
