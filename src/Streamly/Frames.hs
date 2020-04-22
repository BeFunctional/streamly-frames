{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Streamly.Frames
    ( streamTable
    , toTable
    , unquotedCSV
    )
where
import qualified Streamly.Prelude              as S
import           Streamly                       ( IsStream )
import qualified Streamly.Data.Fold            as FL
import           Control.Monad.Catch            ( MonadCatch )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Streamly.Internal.FileSystem.File
                                               as File
import qualified Data.Text                     as T
import           Streamly.Internal.Data.Unicode.Stream
                                                ( decodeUtf8 )
import           Data.Maybe                     ( isNothing )
import           Data.Vinyl
import           Data.Word                      ( Word8 )
import           Data.Vinyl.Functor
import           Frames.CSV                     ( ParserOptions(..)
                                                , QuotingMode(..)
                                                , ReadRec
                                                , readRec
                                                , tokenizeRow
                                                )

-- | Stream a table from a file path.
--
streamTable
    :: (MonadIO m, IsStream t, RMap rs, ReadRec rs, MonadCatch m)
    => ParserOptions
    -> FilePath
    -> t m (Rec (Maybe :. ElField) rs)
streamTable opts src =
    S.map (doParse . tokenizeRow opts)
        . handleHeader
        . S.map T.pack
        . S.splitOnSuffix (== '\n') FL.toList
        . decodeUtf8
        $ File.toBytes src
  where
    handleHeader | isNothing (headerOverride opts) = S.drop 1
                 | otherwise                       = id
    doParse =
        rmap (either (const (Compose Nothing)) (Compose . Just) . getCompose)
            . readRec
{-# INLINE streamTable #-}

-- | Convert a stream of `Word8` to a table by decoding to utf8 and splitting the stream
-- on newline ('\n') characters.
--
toTable
    :: (MonadIO m, IsStream t, RMap rs, ReadRec rs)
    => ParserOptions
    -> t m Word8
    -> t m (Rec (Maybe :. ElField) rs)
toTable opts =
    S.map (doParse . tokenizeRow opts)
        . handleHeader
        . S.map T.pack
        . S.splitOnSuffix (== '\n') FL.toList
        . decodeUtf8
  where
    handleHeader | isNothing (headerOverride opts) = S.drop 1
                 | otherwise                       = id
    doParse =
        rmap (either (const (Compose Nothing)) (Compose . Just) . getCompose)
            . readRec
{-# INLINE toTable #-}

-- | ParserOptions for a CSV file without quoting
--
unquotedCSV :: ParserOptions
unquotedCSV = ParserOptions Nothing "," NoQuoting
