
module ToyPlugin where

import qualified Outputable as O
import GHC
import Control.Monad
import CoreMonad
import DynFlags
import HscTypes
import GHC.Paths (libdir)
import TcRnTypes
import Plugins


plugin :: Plugin
plugin = defaultPlugin { pluginRecompile = \_ -> pure ForceRecompile
                       , typeCheckResultAction = typecheckHook
                       , installCoreToDos   = coreHook
                       }

cleanSummary :: ModSummary -> ModSummary
cleanSummary modSummary = modSummary
  { ms_hspp_opts = updOptLevel 2 (ms_hspp_opts modSummary) { cachedPlugins = [], staticPlugins = [] }
  }

typecheckHook :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckHook _ modSummary gblEnv = do
  let modSum' = cleanSummary modSummary
  coreBinds <- runGhcT (Just libdir) $ do
    parsed      <- parseModule     modSum'
    typechecked <- typecheckModule parsed
    mg_binds . dm_core_module <$> desugarModule typechecked
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
