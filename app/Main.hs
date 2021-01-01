{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-warn-orphans #-}
module Main (main) where
import                            Control.Monad
import                            Data.HashMap.Strict (HashMap)
import qualified                  Data.HashMap.Strict as HM
import                            Data.List (foldl')
import qualified                  Data.List as List
import                            Data.Maybe
import                            Data.Text (Text)
import qualified                  Data.Text as T
import qualified                  Data.Text.IO as T
import "ghc-lib-parser"           DynFlags
import qualified "ghc-lib-parser" EnumSet as ES
import "ghc-lib-parser"           FastString (mkFastString)
import "ghc-lib-parser"           GHC.LanguageExtensions
import qualified "ghc-lib-parser" Lexer as L
import "ghc-lib-parser"           SrcLoc
import "ghc-lib-parser"           StringBuffer
import                            System.Environment

main :: IO ()
main = do
  _:input:output:_ <- getArgs
  contents <- T.readFile input -- gather metadata, transform content
  case tokenizeHaskellLoc contents of
    Nothing -> error "Bad lex!"
    Just tokens -> do
      T.writeFile
        output
        (T.concat
           [ "{-# LINE 1 \"" <> T.pack input <> "\" #-}\n"
           , "{-# OPTIONS -fplugin=EarlyPlugin -fplugin-opt=EarlyPlugin:"
           , T.intercalate
               ","
               (map
                  (\Loc {..} ->
                     T.intercalate ":" (map (T.pack . show) [line, col]))
                  qs)
           , " #-}\n"
           , strip (buildlocs qs) contents
           ])
      where qs = questions tokens

buildlocs :: [Loc] -> HashMap Int [Int]
buildlocs = HM.fromListWith (<>) . map (\Loc{line,col} -> (line,pure col))

-- Keep a running map of lines to cols to delete. Clear the lines
-- after applying them, reducing the map size. Perhaps that's a
-- premature optimization, but it's clean.
strip :: HashMap Int [Int] -> Text -> Text
strip locs0 = T.unlines . snd . List.mapAccumL cut locs0 . zip [1 ..] . T.lines
  where
    cut locs (line, text) =
      if HM.null locs
        then (locs, text)
        else case HM.lookup line locs of
               Nothing -> (locs, text)
               Just cols -> (HM.delete line locs, text')
                 where !text' =
                         foldl'
                           (\text'' col ->
                              T.take (col - 1) text'' <> " " <> T.drop col text'')
                           text
                           cols

questions :: [(L.Token, Maybe t)] -> [t]
questions tokens =
  mapMaybe
    (\((tok, loc), (ntok, _)) -> do
       guard (tok == (L.ITvarsym "?") && isEndOfStatement ntok)
       loc)
    (zip tokens (drop 1 tokens))

-- False negatives are an error, but false positives are fine, they
-- will be rejected in a later stage when more information is
-- available.
--
-- A question-mark can only appear BEFORE the last do statement,
-- therefore the only legitimate token following is a semi! which
-- separates do statements, explicitly or implicitly.
--
-- This would permit also @where x = 1?; y = 2@, but that's fine. It
-- will be flagged up as invalid during the parsing phase in the
-- plugin. We will complain loudly as an error when any remaining ?'s
-- are not resolved during that stage.
--
-- Additionally, it's not in operator position (e.g. x?y); we do not
-- want to pick up valid syntax.
isEndOfStatement :: L.Token -> Bool
isEndOfStatement =
  \case
    L.ITsemi -> True
    _ -> False

deriving instance Eq L.Token
data Loc = Loc
  { line, col :: !Int
  } deriving (Eq, Ord, Show)

tokenizeHaskellLoc :: Text -> Maybe [(L.Token, Maybe Loc)]
tokenizeHaskellLoc input =
  case L.unP pLexer parseState of
    L.PFailed {} -> Nothing
    L.POk _ x -> Just x
  where
    location = mkRealSrcLoc (mkFastString "") 1 1
    buffer = stringToStringBuffer (T.unpack input)
    parseState = L.mkPStatePure parserFlags buffer location
    parserFlags = L.mkParserFlags (foldl' xopt_set initialDynFlags enabledExts)
    initialDynFlags =
      DynFlags
        { warningFlags = ES.empty,
          generalFlags =
            ES.fromList
              [ Opt_Haddock,
                Opt_KeepRawTokenStream
              ],
          extensions = [],
          extensionFlags = ES.empty,
          safeHaskell = Sf_Safe,
          language = Just Haskell2010
        }

pLexer :: L.P [(L.Token, Maybe Loc)]
pLexer = go
  where
    go = do
      r <- L.lexer False return
      case r of
        L _ L.ITeof -> return []
        _ ->
          case fixupToken r of
            x -> (x :) <$> go

fixupToken :: Located L.Token -> (L.Token, Maybe Loc)
fixupToken (L srcSpan tok) = (tok,srcSpanToLoc srcSpan)

srcSpanToLoc :: SrcSpan -> Maybe Loc
srcSpanToLoc (RealSrcSpan rss) =
  let start = realSrcSpanStart rss
   in Just $
      Loc (srcLocLine start) (srcLocCol start)
srcSpanToLoc _ = Nothing

----------------------------------------------------------------------------
-- Language extensions

-- | Language extensions we enable by default.
enabledExts :: [Extension]
enabledExts =
  [ ForeignFunctionInterface,
    InterruptibleFFI,
    CApiFFI,
    Arrows,
    TemplateHaskell,
    TemplateHaskellQuotes,
    ImplicitParams,
    OverloadedLabels,
    ExplicitForAll,
    BangPatterns,
    PatternSynonyms,
    MagicHash,
    RecursiveDo,
    UnicodeSyntax,
    UnboxedTuples,
    UnboxedSums,
    DatatypeContexts,
    TransformListComp,
    QuasiQuotes,
    LambdaCase,
    BinaryLiterals,
    NegativeLiterals,
    HexFloatLiterals,
    TypeApplications,
    StaticPointers,
    NumericUnderscores,
    StarIsType
  ]
