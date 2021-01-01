{-# LANGUAGE PackageImports #-}
module EarlyPlugin (plugin) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable          (for_)
import Data.List              (foldl')
import Data.List.NonEmpty     (NonEmpty (..))
import Data.Traversable       (for)

import qualified Data.Generics as SYB

import qualified "ghc" ErrUtils    as Err
import qualified "ghc" GhcPlugins  as GHC
import           "ghc" HsExtension (GhcPs, NoExt (..))
import           "ghc" HsSyn
import           "ghc" SrcLoc

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.parsedResultAction = \_cliOptions -> pluginImpl
    }

pluginImpl :: GHC.ModSummary -> GHC.HsParsedModule -> GHC.Hsc GHC.HsParsedModule
pluginImpl _modSummary m = do
    dflags <- GHC.getDynFlags
    debug $ GHC.showPpr dflags (GHC.hpm_module m )
    hpm_module' <- transform dflags (GHC.hpm_module m)
    let module' = m { GHC.hpm_module = hpm_module' }
    return module'

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn
-- debug _ = pure ()

transform
    :: GHC.DynFlags
    -> GHC.Located (HsModule GhcPs)
    -> GHC.Hsc (GHC.Located (HsModule GhcPs))
transform dflags = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcPs -> GHC.Hsc (LHsExpr GhcPs)
    transform' e@(L l (HsPar _ (L l' (ExplicitList  _ Nothing exprs)))) | inside l l' =
        case exprs of
            [expr] -> do
                expr' <- transformExpr dflags expr
                return (L l (HsPar NoExt expr'))
            _ -> do
                liftIO $ GHC.putLogMsg dflags GHC.NoReason Err.SevWarning l (GHC.defaultErrStyle dflags) $
                    GHC.text "Non singleton idiom bracket list"
                    GHC.$$
                    GHC.ppr exprs
                return e
    transform' (L l (HsPar _ (L l' (HsDo _ ListComp (L _ stmts)))))
        | inside l l', Just exprs <- matchListComp stmts = do
            for_ exprs $ \expr ->
                debug $ "ALT: " ++ GHC.showPpr dflags expr
--            for_ (zip stmts [0..]) $ \(stmt, i) -> do
--                debug $ show i ++ " ==> " ++ SYB.gshow stmt
            exprs' <- traverse (transformExpr dflags) exprs
            return (foldr1 altExpr exprs')
    transform' expr =
        return expr

-------------------------------------------------------------------------------
-- Expression
-------------------------------------------------------------------------------

transformExpr
    :: MonadIO m
    => GHC.DynFlags
    -> LHsExpr GhcPs
    -> m (LHsExpr GhcPs)
transformExpr dflags expr@(L _e OpApp {}) = do
    let bt = matchOp expr
    let result = idiomBT bt
    debug $ "RES : " ++ GHC.showPpr dflags result
    return result
transformExpr dflags expr = do
    let (f :| args) = matchApp expr
    let f' = pureExpr f
    debug $ "FUN : " ++ GHC.showPpr dflags f
    debug $ "FUN+: " ++ GHC.showPpr dflags f'
    for_ (zip args args) $ \arg ->
        debug $ "ARG : " ++ GHC.showPpr dflags arg
    let result = foldl' apply f' args
    debug $ "RES : " ++ GHC.showPpr dflags result
    return result

-------------------------------------------------------------------------------
-- Pure
-------------------------------------------------------------------------------

-- f ~> pure f
pureExpr :: LHsExpr GhcPs -> LHsExpr GhcPs
pureExpr (L l f) =
    L l $ HsApp NoExt (L l' (HsVar NoExt (L l' pureRdrName))) (L l' f)
  where
    l' = GHC.noSrcSpan

pureRdrName :: GHC.RdrName
pureRdrName = GHC.mkRdrUnqual (GHC.mkVarOcc "pure")

-- x y ~> x <|> y
altExpr :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
altExpr x y =
    L l' $ OpApp NoExt x (L l' (HsVar NoExt (L l' altRdrName))) y
  where
    l' = GHC.noSrcSpan

altRdrName :: GHC.RdrName
altRdrName = GHC.mkRdrUnqual (GHC.mkVarOcc "<|>")

-- f x ~> f <$> x
fmapExpr :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
fmapExpr f x =
    L l' $ OpApp NoExt f (L l' (HsVar NoExt (L l' fmapRdrName))) x
  where
    l' = GHC.noSrcSpan

fmapRdrName :: GHC.RdrName
fmapRdrName = GHC.mkRdrUnqual (GHC.mkVarOcc "<$>")

-- f x ~> f <*> x
apExpr :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
apExpr f x =
    L l' $ OpApp NoExt f (L l' (HsVar NoExt (L l' apRdrName))) x
  where
    l' = GHC.noSrcSpan

apRdrName :: GHC.RdrName
apRdrName = GHC.mkRdrUnqual (GHC.mkVarOcc "<*>")

-- f x -> f <* x
birdExpr :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
birdExpr f x =
    L l' $ OpApp NoExt f (L l' (HsVar NoExt (L l' birdRdrName))) x
  where
    l' = GHC.noSrcSpan

birdRdrName :: GHC.RdrName
birdRdrName = GHC.mkRdrUnqual (GHC.mkVarOcc "<*")

-- f x -y z  ->  (((pure f <*> x) <* y) <*> z)
apply :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
apply f (L _ (HsPar _ (L _ (HsApp _ (L _ (HsVar _ (L _ voidName'))) x))))
    | voidName' == voidName = birdExpr f x
apply f x                   = apExpr f x

voidName :: GHC.RdrName
voidName = GHC.mkRdrUnqual (GHC.mkVarOcc "void")

-------------------------------------------------------------------------------
-- Function application maching
-------------------------------------------------------------------------------

-- | Match nested function applications, 'HsApp':
-- f x y z ~> f :| [x,y,z]
--
matchApp :: LHsExpr p -> NonEmpty (LHsExpr p)
matchApp (L _ (HsApp _ f x)) = neSnoc (matchApp f) x
matchApp e = pure e

neSnoc :: NonEmpty a -> a -> NonEmpty a
neSnoc (x :| xs) y = x :| xs ++ [y]

-------------------------------------------------------------------------------
-- Operator application matching
-------------------------------------------------------------------------------

-- | Match nested operator applications, 'OpApp'.
-- x + y * z ~>  Branch (+) (Leaf x) (Branch (*) (Leaf y) (Leaf z))
matchOp :: LHsExpr p -> BT (LHsExpr p)
matchOp (L _ (OpApp _  lhs op rhs)) = Branch (matchOp lhs) op (matchOp rhs)
matchOp x = Leaf x

-- | Non-empty binary tree, with elements at branches too.
data BT a = Leaf a | Branch (BT a) a (BT a)

-- flatten: note that leaf is returned as is.
idiomBT :: BT (LHsExpr GhcPs) -> LHsExpr GhcPs
idiomBT (Leaf x)            = x
idiomBT (Branch lhs op rhs) = fmapExpr op (idiomBT lhs) `apExpr` idiomBT rhs

-------------------------------------------------------------------------------
-- List Comprehension
-------------------------------------------------------------------------------

matchListComp :: [LStmt GhcPs (LHsExpr GhcPs)] -> Maybe [LHsExpr GhcPs]
matchListComp [L _ (BodyStmt _ expr2 _ _), L _ (LastStmt _ expr1 _ _)] =
    Just [expr1, expr2]
matchListComp [L _ (ParStmt _ blocks _ _), L _ (LastStmt _ expr1 _ _)] = do
    exprs <- for blocks $ \bl -> case bl of
        ParStmtBlock _ [L _ (BodyStmt _ e _ _)] _ _ -> Just e
        _ -> Nothing
    return $ expr1 : exprs
matchListComp _ = Nothing

-------------------------------------------------------------------------------
-- Location checker
-------------------------------------------------------------------------------

-- Check that spans are right inside each others, i.e. we match
-- that there are no spaces between parens and brackets
inside :: SrcSpan -> SrcSpan -> Bool
inside (RealSrcSpan a) (RealSrcSpan b) = and
    [ srcSpanStartLine a == srcSpanStartLine b
    , srcSpanEndLine a == srcSpanEndLine b
    , srcSpanStartCol a + 1 == srcSpanStartCol b
    , srcSpanEndCol a == srcSpanEndCol b + 1
    ]
inside _ _ = False
