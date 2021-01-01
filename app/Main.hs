{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Main (main) where
import System.Environment
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import DynFlags
import qualified EnumSet as ES
import FastString (mkFastString)
import GHC.LanguageExtensions
import qualified Lexer as L
import SrcLoc
import StringBuffer

main :: IO ()
main = do
  _:input:output:_ <- getArgs
  contents <- readFile input -- gather metadata, transform content
  writeFile
    output
    ("{-# LINE 1 \"" <> input <> "\" #-}\n{-# OPTIONS -fplugin=EarlyPlugin #-}") -- pass any extra arguments
  appendFile output contents


data Loc = Loc !Int !Int !Int !Int
  deriving (Eq, Ord, Show)

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
      end = realSrcSpanEnd rss
   in Just $
      Loc (srcLocLine start) (srcLocCol start) (srcLocLine end) (srcLocCol end)
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
