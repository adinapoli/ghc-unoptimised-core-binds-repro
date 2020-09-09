
module ToyPlugin where

import qualified Outputable as O
import HscMain
import TcRnMonad
import GHC
import Control.Monad
import CoreMonad
import CoreSyn
import DynFlags
import HscTypes
import GHC.Paths (libdir)
import Plugins


-- | A simple 'Plugin' that emits on stdout the 'CoreBind's produced by \"turning off optimisations\"
-- and the 'CoreBind's as received in a core pass.
plugin :: Plugin
plugin = defaultPlugin { pluginRecompile       = \_ -> pure ForceRecompile
                       , typeCheckResultAction = typecheckAction
                       , installCoreToDos      = coreAction
                       }

-- | Set the optimization level to 0 (\"-O0\") and clean any plugin to avoid that calling 'desugarModule'
-- might accidentally loop everything.
cleanSummary :: ModSummary -> ModSummary
cleanSummary modSummary = modSummary
  { ms_hspp_opts = unoptFlags (ms_hspp_opts modSummary) { cachedPlugins = [], staticPlugins = [] }
  }

-- | Set the optimization level to 0.
unoptFlags :: DynFlags -> DynFlags
unoptFlags = updOptLevel 0

-- | Yields a new 'HscEnv' with optimisations turned off.
cleanEnv :: HscEnv -> HscEnv
cleanEnv env = env { hsc_dflags = unoptFlags (hsc_dflags env) }

typecheckAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckAction _ modSummary gblEnv = do
  -- Try to produce core binds with optimisations turned off by reusing the 'TcGblEnv'
  -- we got as input.
  let modSum' = cleanSummary modSummary
  env <- cleanEnv <$> getTopEnv
  coreBinds <- runGhcT (Just libdir) $ do
    mg_binds <$> liftIO (hscDesugar env modSum' gblEnv)
  printBinds "Core Binds generated with -O0:" coreBinds
  pure gblEnv

coreAction :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
coreAction _ passes = pure (CoreDoPluginPass "ToyPluginPass" myPass : passes)
  where
    myPass :: ModGuts -> CoreM ModGuts
    myPass guts = do
      oLvl <- getOptLevel
      when (moduleNameString (moduleName . mg_module $ guts) == "Main") $ do
        liftIO $ putStrLn $ "User optimization level: " <> show oLvl
        printBinds "Core Binds at the Core phase:" (mg_binds guts)
      pure guts
    getOptLevel :: (Functor m, HasDynFlags m) => m Int
    getOptLevel = optLevel <$> getDynFlags

--
-- Helpers
--

printBinds :: MonadIO m => String -> [CoreBind] -> m ()
printBinds label binds = liftIO $ do
  putStrLn $ (replicate 80 '*')
  putStrLn label
  putStrLn $ (O.showSDocUnsafe $ O.vcat $ map O.ppr binds)
  putStrLn $ (replicate 80 '*')

