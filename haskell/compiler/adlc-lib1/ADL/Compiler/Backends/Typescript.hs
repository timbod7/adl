{-|
Module : ADL.Compiler.Backends.Typescript
Description: Typescript backend for ADL

This module contains that necessary functions to generate
a typescript backend from an ADL file.
-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript(
 generate,
 generateBatch,
  TypescriptFlags(..),
  TypescriptStyle(..),
  ) where

import           ADL.Compiler.AST
import           ADL.Compiler.Primitive
import           ADL.Utils.FileDiff                         (dirContents)
import           ADL.Utils.Format(template,formatText)
import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.Map                                   as M
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as T

import           ADL.Compiler.EIO
import           ADL.Compiler.Processing
import           ADL.Compiler.Utils
import           ADL.Utils.IndentedCode

import           Control.Monad                              (when)
import           Control.Monad.Trans                        (liftIO)
import           Control.Monad.Trans.State.Strict
import           Data.Foldable                              (for_)
import           Data.List                                  (sortOn)
import           Data.Monoid
import           Data.Traversable                           (for)
import           System.FilePath                            (joinPath,
                                                             takeDirectory,
                                                             (<.>), (</>))

import           ADL.Compiler.Backends.Typescript.Internal
import           ADL.Compiler.Backends.BatchUtils
import           ADL.Compiler.DataFiles
import           ADL.Compiler.ExternalAST                   (scopedNameFromA2)
import           ADL.Adlc.Codegen.Typescript                (TypescriptParams(..))
import qualified ADL.Adlc.Codegen.Typescript                as CG

generateBatch :: FilePath -> TypescriptParams -> EIOT ()
generateBatch libDir params = do
  let log = batchLogFn (typescriptParams_verbose params)
      mloader = batchModuleLoader log (typescriptParams_sources params) (typescriptParams_mergeExts params)
      mlc = ModuleLoadContext mloader log "batch"
      tf = TypescriptFlags {
        tsStyle= case typescriptParams_style params of
          CG.TypescriptStyle_tsc -> Tsc
          CG.TypescriptStyle_deno -> Deno,
        tsLibDir=libDir,
        tsIncludeRuntime=typescriptParams_includeRuntime params,
        tsIncludeResolver=typescriptParams_includeResolver params,
        tsExcludeAst=typescriptParams_excludeAst params,
        tsExcludedAstAnnotations=Just (map scopedNameFromA2 (typescriptParams_excludedAstAnnotations params)),
        tsRuntimeDir=T.unpack (typescriptParams_runtimePath params)
      }
  withBatchFileWriter log (typescriptParams_output params) $ \fileWriter -> do
    (reqmods,allmods) <- findAndCheckModules mlc (map moduleNameFromText (typescriptParams_modules params))
    let genmods = if (typescriptParams_generateTransitive params) then allmods else reqmods
    for genmods $ \m -> do
      let m' = fullyScopedModule m
      when (generateCode (m_annotations m')) (generateModule tf fileWriter m')
      return m'
    when (tsIncludeRuntime tf) (generateRuntime tf fileWriter)
    when (tsIncludeResolver tf) (generateResolver tf fileWriter genmods)

-- | Run this backend on a list of ADL modules. Check each module
-- for validity, and then generate the code for it.
generate :: AdlFlags -> TypescriptFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af tf fileWriter modulePaths = catchAllExceptions  $ do
  (ms0,tms) <- loadAndCheckModules (moduleFinder af) modulePaths
  let ms = if (af_generateTransitive af) then tms else ms0
  for ms $ \m -> do
    let m' = fullyScopedModule m
    when (generateCode (m_annotations m')) (generateModule tf fileWriter m')
    return m'
  when (tsIncludeRuntime tf) (generateRuntime tf fileWriter)
  when (tsIncludeResolver tf) (generateResolver tf fileWriter ms)

-- JS.generate af (JS.JavascriptFlags {}) fileWriter
generateRuntime :: TypescriptFlags -> FileWriter -> EIOT ()
generateRuntime tf fileWriter = do
    files <- liftIO $ dirContents runtimeLibDir
    liftIO $ for_ files $ \inpath -> do
      content0 <- LBS.readFile (runtimeLibDir </> inpath)
      let content = applyTypescriptStyle tf content0
      fileWriter (tsRuntimeDir tf </> inpath) content
    where
      runtimeLibDir = typescriptRuntimeDir (tsLibDir tf)

generateResolver :: TypescriptFlags -> FileWriter -> [RModule] -> EIOT ()
generateResolver tf fileWriter ms = do
  liftIO $ fileWriter "resolver.ts" (genCode code)
  where
    gms = sortOn m_name (filter (generateCode . m_annotations) ms)
    code
      =  cline "/* @generated from adl */"
      <> ctemplate "import { declResolver, ScopedDecl } from \"./$1/adl$2\";" [T.pack (tsRuntimeDir tf), mext]
      <> mconcat [ctemplate "import { _AST_MAP as $1 } from \"./$2$3\";" [moduleNameText m, modulePathText m, mext] | m <- gms]
      <> cline ""
      <> cline "export const ADL: { [key: string]: ScopedDecl } = {"
      <> mconcat [ctemplate "  ...$1," [moduleNameText m] | m <- gms]
      <> cline "};"
      <> cline ""
      <> cline "export const RESOLVER = declResolver(ADL);"
    moduleNameText m = T.intercalate "_" (unModuleName (m_name m))
    modulePathText m = T.intercalate "/" (unModuleName (m_name m))
    mext = moduleExt tf

-- | Generate and the typescript code for a single ADL module, and
-- save the resulting code to the apppropriate file
generateModule :: TypescriptFlags ->
                  FileWriter ->
                  RModule ->
                  EIO T.Text ()
generateModule tf fileWriter m0 = do
  let moduleName = m_name m
      m = associateCustomTypes getCustomType moduleName m0
      cgp = CodeGenProfile {
        cgp_includeAst = not (tsExcludeAst tf),

        cgp_includeAstAnnotation = case tsExcludedAstAnnotations tf of
            Nothing  -> (/=) (ScopedName (ModuleName ["sys", "annotations"]) "Doc")
            (Just sns) -> (\sn -> sn `notElem` sns)
      }
      mf = execState (genModule m) (emptyModuleFile (m_name m) cgp)
  liftIO $ fileWriter (moduleFilePath (unModuleName moduleName) <.> "ts") (genModuleCode tf "adlc" mf)

genModule :: CModule -> CState ()
genModule m = do
  includeAst <- fmap (cgp_includeAst . mfCodeGenProfile) get
  when includeAst $ do
    addImport "ADL" (TSImport "ADL" ["runtime","adl"])

  -- Generate each declaration
  for_ (getOrderedDecls m) $ \decl ->
    when (generateCode (d_annotations decl)) $
      case d_type decl of
        (Decl_Struct struct)   -> genStruct m decl struct
        (Decl_Union union)     -> genUnion m decl union
        (Decl_Typedef typedef) -> genTypedef m decl typedef
        (Decl_Newtype ntype)   -> genNewtype m decl ntype

  when includeAst $ do
    addAstMap m

genStruct :: CModule -> CDecl -> Struct CResolvedType -> CState ()
genStruct m decl struct@Struct{s_typeParams=typeParams0} = do
  fds <- mapM genFieldDetails (s_fields struct)
  let structName = capitalise (d_name decl)
  let typeParams = prefixUnusedTypeParams (isTypeParamUsedInFields (s_fields struct)) typeParams0

  addDeclaration $ renderCommentForDeclaration decl <> renderInterface structName typeParams fds False
  when (not (any isTypeToken fds)) $ do
    addDeclaration $ renderFactory structName typeParams fds
  addAstDeclaration m decl

genUnion :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnion  m decl union@Union{u_typeParams=parameters} = do
  genUnionWithDiscriminate m decl union
  addAstDeclaration m decl

genUnionWithDiscriminate :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnionWithDiscriminate  m decl union
  | isUnionEnum union = genUnionEnum m decl union
  | otherwise = genUnionInterface m decl union

genUnionEnum :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnionEnum _ decl enum = do
  fds <- mapM genFieldDetails (u_fields enum)
  let enumName = capitalise (d_name decl)
      enumValues = ["'" <> fdName fd <> "'" | fd <- fds]
      enumDecl =  cline (template "export type $1 = $2;" [enumName, T.intercalate " | " enumValues])
               <> cline (template "export const values$1 : $1[] = [$2];" [enumName, T.intercalate ", " enumValues])
  addDeclaration enumDecl

genUnionInterface :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnionInterface _ decl union = do
  fds <- mapM genFieldDetails (u_fields union)
  addDeclaration (renderUnionFieldsAsInterfaces decl union fds)
  addDeclaration (renderUnionChoice decl union fds)
  addDeclaration (renderUnionChoicesObject decl union fds)
  addDeclaration (renderUnionFactory decl union fds)

renderUnionChoice :: CDecl -> Union CResolvedType -> [FieldDetails] -> Code
renderUnionChoice decl union fds =
  CAppend renderedComments (ctemplate "export type $1$2 = $3;" [d_name decl, renderedParameters, T.intercalate " | " [getChoiceName fd | fd <- fds]])
  where
    typeParams = prefixUnusedTypeParams (isTypeParamUsedInFields (u_fields union)) (u_typeParams union)
    getChoiceName fd = d_name decl <> "_" <> capitalise (fdName fd) <> renderedParameters
    renderedComments = renderCommentForDeclaration decl
    renderedParameters = typeParamsExpr typeParams

-- Render an interface such that the keys and values give the union kinds and values.
renderUnionChoicesObject :: CDecl -> Union CResolvedType -> [FieldDetails] -> Code
renderUnionChoicesObject decl union fds =
  renderInterface (d_name decl <> "Opts") typeParams fds False
  where
    typeParams = prefixUnusedTypeParams (isTypeParamUsedInFields (u_fields union)) (u_typeParams union)

-- Render makeXxx function for union taking kind & value args
renderUnionFactory :: CDecl -> Union CResolvedType -> [FieldDetails] -> Code
renderUnionFactory decl union _ =
  ctemplate "export function make$1$2(kind: K, value: $1Opts$3[K]) { return {kind, value}; }" [d_name decl, renderedParametersWithK, renderedParameters]
  where
    typeParams = prefixUnusedTypeParams (isTypeParamUsedInFields (u_fields union)) (u_typeParams union)
    renderedParameters = typeParamsExpr typeParams
    renderedParametersWithK = typeParamsExpr (typeParams ++ [T.pack "K extends keyof " <> d_name decl <> T.pack "Opts" <> renderedParameters])

renderUnionFieldsAsInterfaces :: CDecl -> Union CResolvedType -> [FieldDetails] -> Code
renderUnionFieldsAsInterfaces decl union (fd:fds) =
  CAppend renderedInterface (renderUnionFieldsAsInterfaces decl union fds)
    where
      renderedInterface = CAppend (renderInterface interfaceName typeParams fieldDetails False) CEmpty
      typeParams = prefixUnusedTypeParams (isTypeParamUsedInTypeExpr (f_type (fdField fd))) (u_typeParams union)
      interfaceName = d_name decl <> "_" <> capitalise (fdName fd)
      fieldDetails = constructUnionFieldDetailsFromField fd
renderUnionFieldsAsInterfaces _ _ [] = CEmpty

constructUnionFieldDetailsFromField :: FieldDetails -> [FieldDetails]
constructUnionFieldDetailsFromField fd@FieldDetails{fdField=Field{f_type=(TypeExpr (RT_Primitive P_Void) _)}}
 = [FieldDetails{
  fdName="kind",
  fdField=Field{
    f_name="kind",
    f_serializedName="kind",
    f_type=TypeExpr (RT_Primitive P_String) [],
    f_default=Nothing,
    f_annotations=M.empty},
  fdTypeExprStr="'" <> fdName fd <> "'",
  fdOptional=False,
  fdDefValue=Nothing}]
constructUnionFieldDetailsFromField fd = [FieldDetails{
  fdName="kind",
  fdField=Field{
    f_name="kind",
    f_serializedName="kind",
    f_type=TypeExpr (RT_Primitive P_String) [],
    f_default=Nothing,
    f_annotations=M.empty},
  fdTypeExprStr="'" <> fdName fd <> "'",
  fdOptional=False,
  fdDefValue=Nothing},
  FieldDetails{fdName="value",
  fdField=Field{
    f_name="value",
    f_serializedName="value",
    f_type=TypeExpr (RT_Primitive P_String) [],
    f_default=Nothing,
    f_annotations=M.empty},
  fdTypeExprStr=fdTypeExprStr fd,
  fdOptional=False,
  fdDefValue=Nothing}]

genNewtype :: CModule -> CDecl -> Newtype CResolvedType -> CState ()
genNewtype  m decl ntype@Newtype{n_typeParams=typeParams0} = do
  typeExprOutput <- genTypeExpr (n_typeExpr ntype)
  let typeParams = prefixUnusedTypeParams (isTypeParamUsedInTypeExpr (n_typeExpr ntype)) typeParams0
  let
    typeDecl = ctemplate "export type $1$2 = $3;" [d_name decl, typeParamsExpr typeParams, typeExprOutput]
  addDeclaration (renderCommentForDeclaration decl <> typeDecl)
  addAstDeclaration m decl

genTypedef :: CModule -> CDecl -> Typedef CResolvedType -> CState ()
genTypedef m decl typedef@Typedef{t_typeParams=typeParams0} = do
  typeExprOutput <- genTypeExpr (t_typeExpr typedef)
  let typeParams = prefixUnusedTypeParams (isTypeParamUsedInTypeExpr (t_typeExpr typedef)) typeParams0
  let
    typeDecl = ctemplate "export type $1$2 = $3;" [d_name decl, typeParamsExpr typeParams, typeExprOutput]
  addDeclaration (renderCommentForDeclaration decl <> typeDecl)
  addAstDeclaration m decl

