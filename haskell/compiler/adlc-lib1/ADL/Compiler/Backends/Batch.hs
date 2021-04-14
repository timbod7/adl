{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Batch(
  generate
) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Aeson as JSON

import qualified ADL.Compiler.Backends.AST as A
import qualified ADL.Compiler.Backends.Java as J

import Control.Monad.Trans(liftIO)
import Data.Foldable(for_)

import ADL.Compiler.EIO
import ADL.Adlc.Codegen.Batch

import ADL.Core(runJsonParser, decodeAdlParseResult, AdlValue(..), ParseResult(..))

generate :: FilePath -> EIOT ()
generate batchFile = do
  ebatch <- liftIO $ (adlFromJsonFile batchFile :: IO (Either T.Text Batch))
  case ebatch of
    Left err -> eioError err
    Right batch -> for_ batch generateBatchItem

generateBatchItem :: BatchItem -> EIOT ()
generateBatchItem bitem =
  case bitem of
     BatchItem_ast ast -> return ()
     BatchItem_java java -> J.generateBatch java

adlFromJsonFile :: AdlValue a => FilePath -> IO (Either T.Text a)
adlFromJsonFile file = (decodeAdlParseResult from . adlFromJsonByteString) <$> (LBS.readFile file)
  where
    adlFromJsonByteString :: (AdlValue a) => LBS.ByteString -> (ParseResult a)
    adlFromJsonByteString lbs = case JSON.eitherDecode' lbs of
      (Left e) -> ParseFailure ("Invalid Json: " <> T.pack e) []
      (Right jv) -> runJsonParser jsonParser [] jv

    from = " from " <> T.pack file
