{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Brittany.Internal
  ( parsePrintModule
  , parsePrintModuleTests
  , pPrintModule
  , pPrintModuleAndCheck
  , extractCommentConfigs
   -- re-export from utils:
  , parseModule
  , parseModuleFromString
  )
where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint.Parsers

import           Data.Data
import           Control.Monad.Trans.Either
import           Data.HList.HList
import           Data.CZipWith
import qualified UI.Butcher.Monadic as Butcher

import qualified Data.Text.Lazy.Builder as Text.Builder

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Config
import           Language.Haskell.Brittany.Internal.LayouterBasics

import           Language.Haskell.Brittany.Internal.Layouters.Type
import           Language.Haskell.Brittany.Internal.Layouters.Decl
import           Language.Haskell.Brittany.Internal.Utils
import           Language.Haskell.Brittany.Internal.Backend
import           Language.Haskell.Brittany.Internal.BackendUtils
import           Language.Haskell.Brittany.Internal.ExactPrintUtils

import           Language.Haskell.Brittany.Internal.Transformations.Alt
import           Language.Haskell.Brittany.Internal.Transformations.Floating
import           Language.Haskell.Brittany.Internal.Transformations.Par
import           Language.Haskell.Brittany.Internal.Transformations.Columns
import           Language.Haskell.Brittany.Internal.Transformations.Indent

import qualified GHC as GHC hiding (parseModule)
import           ApiAnnotation ( AnnKeywordId(..) )
import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           SrcLoc ( SrcSpan )
import           HsSyn
import qualified DynFlags as GHC
import qualified GHC.LanguageExtensions.Type as GHC

import           Data.Char (isSpace)



data InlineConfigTarget
    = InlineConfigTargetModule
    | InlineConfigTargetNext
    | InlineConfigTargetBinding String

extractCommentConfigs
  :: ExactPrint.Types.Anns -> Either (String, String) InlineConfig
extractCommentConfigs anns = do
  let commentLiness =
        [ ( k
          , [ x
            | (ExactPrint.Types.Comment x _ _, _) <-
              ExactPrint.Types.annPriorComments ann
                ++ ExactPrint.Types.annFollowingComments ann
            ]
          )
        | (k, ann) <- Map.toList anns
        ]
  let
    configLiness = commentLiness <&> second
      ( \lines -> flip Data.Maybe.mapMaybe lines $ \line ->
        List.stripPrefix "-- BRITTANY" line
          <|> (List.stripPrefix "{- BRITTANY " line >>= stripSuffix "-}")
      )
  let parser = do -- we will (mis?)use butcher here to parse the inline config
                  -- line.
        Butcher.addCmd "@" $ do
          Butcher.addCmd "module" $ do
            conf <- configParser
            Butcher.addCmdImpl (InlineConfigTargetModule, conf)
          Butcher.addCmd "vvv" $ do
            conf <- configParser
            Butcher.addCmdImpl (InlineConfigTargetNext, conf)
  lineConfigss <- configLiness `forM` \(k, ss) -> do
    r <- ss `forM` \s -> case Butcher.runCmdParserSimple s parser of
      Left  err -> Left $ (err, s)
      Right c   -> Right $ c
    pure (k, r)

  pure $ InlineConfig
    { _icd_perModule  = foldl' (<>) mempty
        [ conf
        | (_                       , lineConfigs) <- lineConfigss
        , (InlineConfigTargetModule, conf       ) <- lineConfigs
        ]
    , _icd_perBinding = Map.map (foldr1 (<>)) $ Map.fromListWith
      (++)
      [ (s, [conf])
      | (_                          , lineConfigs) <- lineConfigss
      , (InlineConfigTargetBinding s, conf       ) <- lineConfigs
      ]
    , _icd_perKey     = flip Map.mapMaybe (Map.fromList lineConfigss)
      $ \lineConfigs ->
          case [ conf | (InlineConfigTargetNext, conf) <- lineConfigs ] of
            [] -> Nothing
            cs -> Just $ foldr1 (<>) cs
    }


-- | Exposes the transformation in an pseudo-pure fashion. The signature
-- contains `IO` due to the GHC API not exposing a pure parsing function, but
-- there should be no observable effects.
--
-- Note that this function ignores/resets all config values regarding
-- debugging, i.e. it will never use `trace`/write to stderr.
parsePrintModule :: Config -> Text -> IO (Either [BrittanyError] Text)
parsePrintModule config inputText = runEitherT $ do
  let ghcOptions = config & _conf_forward & _options_ghc & runIdentity
  let config_pp          = config & _conf_preprocessor
  let cppMode            = config_pp & _ppconf_CPPMode & confUnpack
  let hackAroundIncludes = config_pp & _ppconf_hackAroundIncludes & confUnpack
  (anns, parsedSource, hasCPP) <- do
    let hackF s = if "#include" `isPrefixOf` s
          then "-- BRITTANY_INCLUDE_HACK " ++ s
          else s
    let hackTransform = if hackAroundIncludes
          then List.intercalate "\n" . fmap hackF . lines'
          else id
    let cppCheckFunc dynFlags = if GHC.xopt GHC.Cpp dynFlags
          then case cppMode of
            CPPModeAbort  -> return $ Left "Encountered -XCPP. Aborting."
            CPPModeWarn   -> return $ Right True
            CPPModeNowarn -> return $ Right True
          else return $ Right False
    parseResult <- lift $ parseModuleFromString
      ghcOptions
      "stdin"
      cppCheckFunc
      (hackTransform $ Text.unpack inputText)
    case parseResult of
      Left  err -> left $ [ErrorInput err]
      Right x   -> pure $ x
  inlineConf <-
    either (left . (:[]) . uncurry ErrorMacroConfig) pure
      $ extractCommentConfigs
          anns
  -- let config = cZipWith fromOptionIdentity configRaw (_icd_perModule inlineConf)
  -- (configRaw { _conf_debug = _conf_debug staticDefaultConfig })
  (errsWarns, outputTextL) <- do
    let omitCheck =
          config
            & _conf_errorHandling
            & _econf_omit_output_valid_check
            & confUnpack
    (ews, outRaw) <- if hasCPP || omitCheck
      then return $ pPrintModule config inlineConf anns parsedSource
      else lift $ pPrintModuleAndCheck config inlineConf anns parsedSource
    let hackF s = fromMaybe s
          $ TextL.stripPrefix (TextL.pack "-- BRITTANY_INCLUDE_HACK ") s
    pure $ if hackAroundIncludes
      then
        ( ews
        , TextL.intercalate (TextL.pack "\n") $ fmap hackF $ TextL.splitOn
          (TextL.pack "\n")
          outRaw
        )
      else (ews, outRaw)
  let customErrOrder ErrorInput{}         = 4
      customErrOrder LayoutWarning{}      = 0 :: Int
      customErrOrder ErrorOutputCheck{}   = 1
      customErrOrder ErrorUnusedComment{} = 2
      customErrOrder ErrorUnknownNode{}   = 3
      customErrOrder ErrorMacroConfig{}   = 5
  let hasErrors =
        case config & _conf_errorHandling & _econf_Werror & confUnpack of
          False -> 0 < maximum (-1 : fmap customErrOrder errsWarns)
          True  -> not $ null errsWarns
  if hasErrors then left $ errsWarns else pure $ TextL.toStrict outputTextL



-- BrittanyErrors can be non-fatal warnings, thus both are returned instead
-- of an Either.
-- This should be cleaned up once it is clear what kinds of errors really
-- can occur.
pPrintModule
  :: Config
  -> InlineConfig
  -> ExactPrint.Types.Anns
  -> GHC.ParsedSource
  -> ([BrittanyError], TextL.Text)
pPrintModule conf inlineConf anns parsedModule =
  let ((out, errs), debugStrings) =
        runIdentity
          $ MultiRWSS.runMultiRWSTNil
          $ MultiRWSS.withMultiWriterAW
          $ MultiRWSS.withMultiWriterAW
          $ MultiRWSS.withMultiWriterW
          $ MultiRWSS.withMultiReader anns
          $ MultiRWSS.withMultiReader conf
          $ MultiRWSS.withMultiReader inlineConf
          $ MultiRWSS.withMultiReader (extractToplevelAnns parsedModule anns)
          $ do
              traceIfDumpConf "bridoc annotations raw" _dconf_dump_annotations
                $ annsDoc anns
              ppModule parsedModule
      tracer = if Seq.null debugStrings
        then id
        else
          trace ("---- DEBUGMESSAGES ---- ")
            . foldr (seq . join trace) id debugStrings
  in  tracer $ (errs, Text.Builder.toLazyText out)
  -- unless () $ do
  --   
  --   debugStrings `forM_` \s ->
  --     trace s $ return ()

-- | Additionally checks that the output compiles again, appending an error
-- if it does not.
pPrintModuleAndCheck
  :: Config
  -> InlineConfig
  -> ExactPrint.Types.Anns
  -> GHC.ParsedSource
  -> IO ([BrittanyError], TextL.Text)
pPrintModuleAndCheck conf inlineConf anns parsedModule = do
  let ghcOptions     = conf & _conf_forward & _options_ghc & runIdentity
  let (errs, output) = pPrintModule conf inlineConf anns parsedModule
  parseResult <- parseModuleFromString ghcOptions
                                       "output"
                                       (\_ -> return $ Right ())
                                       (TextL.unpack output)
  let errs' = errs ++ case parseResult of
        Left{}  -> [ErrorOutputCheck]
        Right{} -> []
  return (errs', output)


-- used for testing mostly, currently.
-- TODO: use parsePrintModule instead and remove this function.
parsePrintModuleTests :: Config -> String -> Text -> IO (Either String Text)
parsePrintModuleTests conf filename input = do
  let inputStr = Text.unpack input
  parseResult <- ExactPrint.Parsers.parseModuleFromString filename inputStr
  case parseResult of
    Left  (_   , s           ) -> return $ Left $ "parsing error: " ++ s
    Right (anns, parsedModule) -> runEitherT $ do
      inlineConf <- case extractCommentConfigs anns of
        Left err -> left $ "error in inline config: " ++ show err
        Right x -> pure x
      let omitCheck =
            conf
              &  _conf_errorHandling
              .> _econf_omit_output_valid_check
              .> confUnpack
      (errs, ltext) <- if omitCheck
        then return $ pPrintModule conf inlineConf anns parsedModule
        else lift $ pPrintModuleAndCheck conf inlineConf anns parsedModule
      if null errs
        then pure $ TextL.toStrict $ ltext
        else
          let
            errStrs = errs <&> \case
              ErrorInput         str -> str
              ErrorUnusedComment str -> str
              LayoutWarning      str -> str
              ErrorUnknownNode str _ -> str
              ErrorMacroConfig str _ -> "when parsing inline config: " ++ str
              ErrorOutputCheck       -> "Output is not syntactically valid."
          in
            left $ "pretty printing error(s):\n" ++ List.unlines errStrs


-- this approach would work if there was a pure GHC.parseDynamicFilePragma.
-- Unfortunately that does not exist yet, so we cannot provide a nominally
-- pure interface.

-- parsePrintModuleTests :: Text -> Either String Text
-- parsePrintModuleTests input = do
--   let dflags = GHC.unsafeGlobalDynFlags
--   let fakeFileName = "SomeTestFakeFileName.hs"
--   let pragmaInfo = GHC.getOptions
--         dflags
--         (GHC.stringToStringBuffer $ Text.unpack input)
--         fakeFileName
--   (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags pragmaInfo
--   let parseResult = ExactPrint.Parsers.parseWith
--         dflags1
--         fakeFileName
--         GHC.parseModule
--         inputStr
--   case parseResult of
--     Left (_, s) -> Left $ "parsing error: " ++ s
--     Right (anns, parsedModule) -> do
--       let (out, errs) = runIdentity
--                       $ runMultiRWSTNil
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiWriterAW
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiWriterW
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiReader anns
--                       $ ppModule parsedModule
--       if (not $ null errs)
--         then do
--           let errStrs = errs <&> \case
--                 ErrorUnusedComment str -> str
--           Left $ "pretty printing error(s):\n" ++ List.unlines errStrs
--         else return $ TextL.toStrict $ Text.Builder.toLazyText out

ppModule :: GenLocated SrcSpan (HsModule RdrName) -> PPM ()
ppModule lmod@(L loc m@(HsModule _name _exports _imports decls _ _)) = do
  let emptyModule = L loc m { hsmodDecls = [] }

  (anns', post) <- do
    anns <- mAsk
    -- evil partiality. but rather unlikely.
    return $ case Map.lookup (ExactPrint.Types.mkAnnKey lmod) anns of
      Nothing -> (anns, [])
      Just mAnn ->
        let modAnnsDp = ExactPrint.Types.annsDP mAnn
            isWhere (ExactPrint.Types.G AnnWhere) = True
            isWhere _                             = False
            isEof (ExactPrint.Types.G AnnEofPos) = True
            isEof _                              = False
            whereInd    = List.findIndex (isWhere . fst) modAnnsDp
            eofInd      = List.findIndex (isEof . fst) modAnnsDp
            (pre, post) = case (whereInd, eofInd) of
              (Nothing, Nothing) -> ([], modAnnsDp)
              (Just i , Nothing) -> List.splitAt (i + 1) modAnnsDp
              (Nothing, Just _i) -> ([], modAnnsDp)
              (Just i , Just j ) -> List.splitAt (min (i + 1) j) modAnnsDp
            mAnn' = mAnn { ExactPrint.Types.annsDP = pre }
            anns' = Map.insert (ExactPrint.Types.mkAnnKey lmod) mAnn' anns
        in  (anns', post)

  MultiRWSS.withMultiReader anns' $ processDefault emptyModule
  decls `forM_` \decl -> do
    let declAnnKey = ExactPrint.Types.mkAnnKey decl
    inlineConf <- mAsk
    let inlineModConf = _icd_perModule inlineConf
    let mDeclConf = Map.lookup declAnnKey $ _icd_perKey inlineConf
    filteredAnns <- mAsk <&> \annMap ->
      Map.findWithDefault Map.empty declAnnKey annMap

    traceIfDumpConf "bridoc annotations filtered/transformed"
                    _dconf_dump_annotations
      $ annsDoc filteredAnns

    config <- mAsk

    let config' = cZipWith fromOptionIdentity config $ case mDeclConf of
          Nothing -> inlineModConf
          Just c -> inlineModConf <> c

    MultiRWSS.withoutMultiReader $ MultiRWSS.withoutMultiReader $ do
      MultiRWSS.mPutRawR $ config' :+: filteredAnns :+: HNil
      ppDecl decl
  let finalComments = filter
        ( fst .> \case
          ExactPrint.Types.AnnComment{} -> True
          _                             -> False
        )
        post
  post `forM_` \case
    (ExactPrint.Types.AnnComment (ExactPrint.Types.Comment cmStr _ _), l) -> do
      ppmMoveToExactLoc l
      mTell $ Text.Builder.fromString cmStr
    (ExactPrint.Types.G AnnEofPos, (ExactPrint.Types.DP (eofX, eofY))) ->
      let folder (acc, _) (kw, ExactPrint.Types.DP (x, y)) = case kw of
            ExactPrint.Types.AnnComment cm
              | GHC.RealSrcSpan span <- ExactPrint.Types.commentIdentifier cm
              -> ( acc + x + GHC.srcSpanEndLine span - GHC.srcSpanStartLine span
                 , y + GHC.srcSpanEndCol span - GHC.srcSpanStartCol span
                 )
            _ -> (acc + x, y)
          (cmX, cmY) = foldl' folder (0, 0) finalComments
      in  ppmMoveToExactLoc $ ExactPrint.Types.DP (eofX - cmX, eofY - cmY)
    _ -> return ()

withTransformedAnns :: Data ast => ast -> PPMLocal () -> PPMLocal ()
withTransformedAnns ast m = do
  -- TODO: implement `local` for MultiReader/MultiRWS
  readers@(conf:+:anns:+:HNil) <- MultiRWSS.mGetRawR
  MultiRWSS.mPutRawR (conf :+: f anns :+: HNil)
  m
  MultiRWSS.mPutRawR readers
 where
  f anns =
    let ((), (annsBalanced, _), _) =
          ExactPrint.runTransform anns (commentAnnFixTransformGlob ast)
    in  annsBalanced


ppDecl :: LHsDecl RdrName -> PPMLocal ()
ppDecl d@(L loc decl) = case decl of
  SigD sig -> -- trace (_sigHead sig) $
              withTransformedAnns d $ do
    -- runLayouter $ Old.layoutSig (L loc sig)
    briDoc <- briDocMToPPM $ layoutSig (L loc sig)
    layoutBriDoc briDoc
  ValD bind -> -- trace (_bindHead bind) $
               withTransformedAnns d $ do
    -- Old.layoutBind (L loc bind)
    briDoc <- briDocMToPPM $ do
      eitherNode <- layoutBind (L loc bind)
      case eitherNode of
        Left  ns -> docLines $ return <$> ns
        Right n  -> return n
    layoutBriDoc briDoc
  _ -> briDocMToPPM (briDocByExactNoComment d) >>= layoutBriDoc

_sigHead :: Sig RdrName -> String
_sigHead = \case
  TypeSig names _ ->
    "TypeSig " ++ intercalate "," (Text.unpack . lrdrNameToText <$> names)
  _ -> "unknown sig"

_bindHead :: HsBind RdrName -> String
_bindHead = \case
  FunBind fId _ _ _ [] -> "FunBind " ++ (Text.unpack $ lrdrNameToText $ fId)
  PatBind _pat _ _ _ ([], []) -> "PatBind smth"
  _                           -> "unknown bind"



layoutBriDoc :: BriDocNumbered -> PPMLocal ()
layoutBriDoc briDoc = do
  -- first step: transform the briDoc.
  briDoc' <- MultiRWSS.withMultiStateS BDEmpty $ do
    -- Note that briDoc is BriDocNumbered, but state type is BriDoc.
    -- That's why the alt-transform looks a bit special here.
    traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw
      $ briDocToDoc
      $ unwrapBriDocNumbered
      $ briDoc
    -- bridoc transformation: remove alts
    transformAlts briDoc >>= mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-alt" _dconf_dump_bridoc_simpl_alt
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyFloating .> mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-floating"
                          _dconf_dump_bridoc_simpl_floating
    -- bridoc transformation: par removal
    mGet >>= transformSimplifyPar .> mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-par" _dconf_dump_bridoc_simpl_par
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyColumns .> mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-columns" _dconf_dump_bridoc_simpl_columns
    -- bridoc transformation: indent
    mGet >>= transformSimplifyIndent .> mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-indent" _dconf_dump_bridoc_simpl_indent
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final
    -- -- convert to Simple type
    -- simpl <- mGet <&> transformToSimple
    -- return simpl

  anns :: ExactPrint.Types.Anns <- mAsk

  let state = LayoutState
        { _lstate_baseYs           = [0]
        , _lstate_curYOrAddNewline = Right 0 -- important that we use left here
                                             -- because moveToAnn stuff of the
                                             -- first node needs to do its
                                             -- thing properly.
        , _lstate_indLevels        = [0]
        , _lstate_indLevelLinger   = 0
        , _lstate_comments         = anns
        , _lstate_commentCol       = Nothing
        , _lstate_addSepSpace      = Nothing
        }

  state' <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'

  let remainingComments =
        extractAllComments =<< Map.elems (_lstate_comments state')
  remainingComments
    `forM_` (fst .> show .> ErrorUnusedComment .> (:[]) .> mTell)

  return $ ()
