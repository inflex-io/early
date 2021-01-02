{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module EarlyPlugin (plugin) where

import                 Control.Monad.IO.Class (MonadIO (..))
import qualified       Data.Generics as SYB
import                 Data.Text (Text)
import qualified       Data.Text as T
import qualified "ghc" GhcPlugins as GHC
import "ghc"           HsExtension (GhcPs)
import "ghc"           HsSyn
import "ghc"           OccName
import "ghc"           SrcLoc
import                 Text.Read

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.parsedResultAction = \cliOptions -> pluginImpl cliOptions
    }

pluginImpl :: [GHC.CommandLineOption] -> GHC.ModSummary -> GHC.HsParsedModule -> GHC.Hsc GHC.HsParsedModule
pluginImpl options _modSummary m = do
  case parseLocs (foldMap T.pack options) of
    Left err -> error err
    Right [] -> pure m
    Right locs -> do
      dflags <- GHC.getDynFlags
      -- debug $ GHC.showPpr dflags (GHC.hpm_module m)
      -- debug "===>"
      hpm_module' <- transform locs dflags (GHC.hpm_module m)
      debug $ show locs
      -- debug $ GHC.showPpr dflags (hpm_module')
      let module' = m {GHC.hpm_module = hpm_module'}
      return module'

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn
-- debug _ = pure ()

transform ::
     [Loc]
  -> GHC.DynFlags
  -> GHC.Located (HsModule GhcPs)
  -> GHC.Hsc (GHC.Located (HsModule GhcPs))
transform locs dflags = SYB.everywhereM (SYB.mkM (transformDo dflags locs))

transformDo :: GHC.DynFlags -> [Loc] -> LHsExpr GhcPs -> GHC.Hsc (LHsExpr GhcPs)
transformDo dflags locs =
  \case
    (L l (HsDo xdo DoExpr (L l' stmts@(_:_)))) ->
      pure (L l (HsDo xdo DoExpr (L l' (transformStmts dflags locs stmts))))
    e -> pure e

transformStmts :: GHC.DynFlags -> [Loc] -> [LStmt GhcPs (LHsExpr GhcPs)] -> [LStmt GhcPs (LHsExpr GhcPs)]
transformStmts _ _ [] = []
transformStmts dflags locs (current:rest)
  | stmtIsEarly locs current =
    transformStmt current (transformStmts dflags locs rest)
  | otherwise = current : transformStmts dflags locs rest

transformStmt ::
     LStmt GhcPs (LHsExpr GhcPs)
  -> [LStmt GhcPs (LHsExpr GhcPs)]
  -> [LStmt GhcPs (LHsExpr GhcPs)]
transformStmt (L stmtloc current) rest =
  case current of
    BodyStmt x lexpr l r ->
      [ L stmtloc
          (BodyStmt
             x
             (L GHC.noSrcSpan
                (HsApp
                   NoExt
                   (L GHC.noSrcSpan
                      (HsApp
                         NoExt
                         (L GHC.noSrcSpan
                            (HsVar NoExt (L GHC.noSrcSpan earlyThenName)))
                         lexpr))
                   (L GHC.noSrcSpan (HsDo NoExt DoExpr (L GHC.noSrcSpan rest)))))
             l
             r)
      ]
    _ -> L stmtloc current : rest

stmtIsEarly :: [Loc] -> LStmt GhcPs (LHsExpr GhcPs) -> Bool
stmtIsEarly locs (L l BindStmt {}) = any (flip srcSpanFollowedBy l) locs
stmtIsEarly locs (L l BodyStmt {}) = any (flip srcSpanFollowedBy l) locs
stmtIsEarly _ _ = False

--------------------------------------------------------------------------------
-- Names

earlyName :: GHC.RdrName
earlyName = GHC.mkQual OccName.varName ("Control.Early","early")

earlyThenName :: GHC.RdrName
earlyThenName = GHC.mkQual OccName.varName ("Control.Early","earlyThen")

--------------------------------------------------------------------------------
-- Locations

srcSpanFollowedBy :: Loc -> SrcSpan -> Bool
srcSpanFollowedBy (Loc line col) sp =
  case sp of
    RealSrcSpan s -> srcSpanEndLine s == line + 1 && srcSpanEndCol s == col
    _ -> False

data Loc = Loc
  { line, col :: !Int
  } deriving (Eq, Ord, Show)

parseLocs :: Text -> Either String [Loc]
parseLocs =
  mapM
    ((\case
        [x, y] -> do
          line <- readEither (T.unpack x)
          col <- readEither (T.unpack y)
          pure (Loc {line, col})
        _ -> Left "Expected line:col pattern for input.") .
     T.splitOn ":") .
  T.splitOn ","
