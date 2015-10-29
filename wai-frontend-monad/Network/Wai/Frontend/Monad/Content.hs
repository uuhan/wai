{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Network.Wai.Frontend.Monad.Content
    ( -- * Content
      Content (..)
    , emptyContent
    , ToContent (..)
    --, ToFlushBuilder (..) FIXME
      -- * Mime types
      -- ** Data type
    , ContentType
    , typeHtml
    , typePlain
    , typeJson
    , typeXml
    , typeAtom
    , typeRss
    , typeJpeg
    , typePng
    , typeGif
    , typeSvg
    , typeJavascript
    , typeCss
    , typeFlv
    , typeOgv
    , typeOctet
      -- * Utilities
    , simpleContentType
    , contentTypeTypes
      -- * Evaluation strategy
    , DontFullyEvaluate (..)
      -- * Representations
    , TypedContent (..)
    , ToTypedContent (..)
    , HasContentType (..)
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text, pack)
import qualified Data.Text as T
import Control.Monad (liftM)

import Data.ByteString.Lazy.Builder (Builder, byteString, lazyByteString, stringUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Monoid
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Control.Monad.Trans.Resource (ResourceT)

import qualified Data.Aeson as J
import Data.Aeson.Encode (encodeToByteStringBuilder)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Word8 (_semicolon, _slash)
import Network.Wai (FilePart)
import Text.Blaze.Html (Html)

-- | Zero-length enumerator.
emptyContent :: Content
emptyContent = ContentBuilder mempty $ Just 0

-- | Anything which can be converted into 'Content'. Most of the time, you will
-- want to use the 'ContentBuilder' constructor. An easier approach will be to use
-- a pre-defined 'toContent' function, such as converting your data into a lazy
-- bytestring and then calling 'toContent' on that.
--
-- Please note that the built-in instances for lazy data structures ('String',
-- lazy 'L.ByteString', lazy 'Text' and 'Html') will not automatically include
-- the content length for the 'ContentBuilder' constructor.
class ToContent a where
    toContent :: a -> Content

instance ToContent Content where
    toContent = id
instance ToContent Builder where
    toContent = flip ContentBuilder Nothing
instance ToContent B.ByteString where
    toContent bs = ContentBuilder (byteString bs) $ Just $ B.length bs
instance ToContent L.ByteString where
    toContent = flip ContentBuilder Nothing . lazyByteString
instance ToContent T.Text where
    toContent = toContent . TE.encodeUtf8Builder
instance ToContent Text where
    toContent = toContent . TLE.encodeUtf8Builder
instance ToContent String where
    toContent = toContent . stringUtf8
instance ToContent Html where
    toContent bs = ContentBuilder (renderHtmlBuilder bs) Nothing
instance ToContent () where
    toContent () = toContent B.empty
instance ToContent (ContentType, Content) where
    toContent = snd
instance ToContent TypedContent where
    toContent (TypedContent _ c) = c

{- FIXME
instance ToFlushBuilder builder => ToContent (Source (ResourceT IO) builder) where
    toContent src = ContentSource $ mapOutput toFlushBuilder src
instance ToFlushBuilder builder => ToContent (ResumableSource (ResourceT IO) builder) where
    toContent (ResumableSource src _) = toContent src

-- | A class for all data which can be sent in a streaming response. Note that
-- for textual data, instances must use UTF-8 encoding.
--
-- Since 1.2.0
class ToFlushBuilder a where toFlushBuilder :: a -> Flush Builder
instance ToFlushBuilder (Flush Builder) where toFlushBuilder = id
instance ToFlushBuilder Builder where toFlushBuilder = Chunk
instance ToFlushBuilder (Flush B.ByteString) where toFlushBuilder = fmap byteString
instance ToFlushBuilder B.ByteString where toFlushBuilder = Chunk . byteString
instance ToFlushBuilder (Flush L.ByteString) where toFlushBuilder = fmap lazyByteString
instance ToFlushBuilder L.ByteString where toFlushBuilder = Chunk . lazyByteString
instance ToFlushBuilder (Flush Text) where toFlushBuilder = fmap Blaze.fromLazyText
instance ToFlushBuilder Text where toFlushBuilder = Chunk . Blaze.fromLazyText
instance ToFlushBuilder (Flush T.Text) where toFlushBuilder = fmap Blaze.fromText
instance ToFlushBuilder T.Text where toFlushBuilder = Chunk . Blaze.fromText
instance ToFlushBuilder (Flush String) where toFlushBuilder = fmap Blaze.fromString
instance ToFlushBuilder String where toFlushBuilder = Chunk . Blaze.fromString
instance ToFlushBuilder (Flush Html) where toFlushBuilder = fmap renderHtmlBuilder
instance ToFlushBuilder Html where toFlushBuilder = Chunk . renderHtmlBuilder
-}

class ToTypedContent a => HasContentType a where
    getContentType :: Monad m => m a -> ContentType

typeHtml :: ContentType
typeHtml = "text/html; charset=utf-8"

typePlain :: ContentType
typePlain = "text/plain; charset=utf-8"

typeJson :: ContentType
typeJson = "application/json; charset=utf-8"

typeXml :: ContentType
typeXml = "text/xml"

typeAtom :: ContentType
typeAtom = "application/atom+xml"

typeRss :: ContentType
typeRss = "application/rss+xml"

typeJpeg :: ContentType
typeJpeg = "image/jpeg"

typePng :: ContentType
typePng = "image/png"

typeGif :: ContentType
typeGif = "image/gif"

typeSvg :: ContentType
typeSvg = "image/svg+xml"

typeJavascript :: ContentType
typeJavascript = "text/javascript; charset=utf-8"

typeCss :: ContentType
typeCss = "text/css; charset=utf-8"

typeFlv :: ContentType
typeFlv = "video/x-flv"

typeOgv :: ContentType
typeOgv = "video/ogg"

typeOctet :: ContentType
typeOctet = "application/octet-stream"

-- | Removes \"extra\" information at the end of a content type string. In
-- particular, removes everything after the semicolon, if present.
--
-- For example, \"text/html; charset=utf-8\" is commonly used to specify the
-- character encoding for HTML data. This function would return \"text/html\".
simpleContentType :: ContentType -> ContentType
simpleContentType = fst . B.break (== _semicolon)

-- Give just the media types as a pair.
-- For example, \"text/html; charset=utf-8\" returns ("text", "html")
contentTypeTypes :: ContentType -> (B.ByteString, B.ByteString)
contentTypeTypes ct = (main, fst $ B.break (== _semicolon) (tailEmpty sub))
  where
    tailEmpty x = if B.null x then "" else B.tail x
    (main, sub) = B.break (== _slash) ct

instance HasContentType a => HasContentType (DontFullyEvaluate a) where
    getContentType = getContentType . liftM unDontFullyEvaluate

instance ToContent a => ToContent (DontFullyEvaluate a) where
    toContent (DontFullyEvaluate a) = ContentDontEvaluate $ toContent a

instance ToContent J.Value where
    toContent = flip ContentBuilder Nothing . encodeToByteStringBuilder
instance HasContentType J.Value where
    getContentType _ = typeJson

instance HasContentType Html where
    getContentType _ = typeHtml

instance HasContentType Text where
    getContentType _ = typePlain

instance HasContentType T.Text where
    getContentType _ = typePlain

-- | Any type which can be converted to 'TypedContent'.
--
-- Since 1.2.0
class ToContent a => ToTypedContent a where
    toTypedContent :: a -> TypedContent

instance ToTypedContent TypedContent where
    toTypedContent = id
instance ToTypedContent () where
    toTypedContent () = TypedContent typePlain (toContent ())
instance ToTypedContent (ContentType, Content) where
    toTypedContent (ct, content) = TypedContent ct content
instance ToTypedContent Html where
    toTypedContent h = TypedContent typeHtml (toContent h)
instance ToTypedContent J.Value where
    toTypedContent v = TypedContent typeJson (toContent v)
instance ToTypedContent T.Text where
    toTypedContent t = TypedContent typePlain (toContent t)
instance ToTypedContent [Char] where
    toTypedContent = toTypedContent . pack
instance ToTypedContent Text where
    toTypedContent t = TypedContent typePlain (toContent t)
instance ToTypedContent a => ToTypedContent (DontFullyEvaluate a) where
    toTypedContent (DontFullyEvaluate a) =
        let TypedContent ct c = toTypedContent a
         in TypedContent ct (ContentDontEvaluate c)

data Content = ContentBuilder !Builder !(Maybe Int) -- ^ The content and optional content length.
             | ContentStream !((Builder -> IO ()) -> IO () -> IO ())
             | ContentFile !FilePath !(Maybe FilePart)
             | ContentDontEvaluate !Content

data TypedContent = TypedContent !ContentType !Content

type ContentType = B.ByteString -- FIXME Text?

-- | Prevents a response body from being fully evaluated before sending the
-- request.
--
-- Since 1.1.0
newtype DontFullyEvaluate a = DontFullyEvaluate { unDontFullyEvaluate :: a }
