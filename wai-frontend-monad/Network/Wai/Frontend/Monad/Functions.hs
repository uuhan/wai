{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Frontend.Monad.Functions where

import Network.Wai.Frontend.Monad.Internal
import Network.Wai
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Functor.Constant
import Data.Functor.Identity
import Control.Monad.Reader
import Control.Monad
import Data.Maybe
import           Network.Wai.Middleware.HttpAuth
    ( extractBasicAuth, extractBearerAuth )
import           Data.Text.Encoding            (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error      (lenientDecode)
import Data.CaseInsensitive (CI)
import qualified Data.ByteString               as S
import qualified Data.ByteString.Char8         as S8
import qualified Text.Blaze.Html.Renderer.Text as RenderText
import qualified Data.Text.Lazy                as TL
import Text.Blaze.Html (Html)
import qualified Data.Map as Map
import qualified Data.Word8 as W8
import qualified Data.Text                     as T
import           Network.Wai.Frontend.Monad.Util      (formatRFC1123)
import Data.Time
import Web.Cookie

askWaiRequest :: (MonadReader env m, HasWaiRequest env) => m Request
askWaiRequest = asks (view waiRequest)

askRichRequest :: (MonadReader env m, HasRichRequest env) => m RichRequest
askRichRequest = asks (view richRequest)

-- | Get the list of supported languages supplied by the user.
--
-- Languages are determined based on the following three (in descending order
-- of preference):
--
-- * The _LANG get parameter.
--
-- * The _LANG cookie.
--
-- * The _LANG user session variable.
--
-- * Accept-Language HTTP header.
--
-- Yesod will seek the first language from the returned list matched with languages supporting by your application. This language will be used to render i18n templates.
-- If a matching language is not found the default language will be used.
--
-- This is handled by parseWaiRequest (not exposed).
languages :: (MonadReader env m, HasRichRequest env) => m [Text]
languages = rrLangs `liftM` askRichRequest

lookup' :: Eq a => a -> [(a, b)] -> [b]
lookup' a = map snd . filter (\x -> a == fst x)

-- | Lookup a request header.
--
-- Since 1.2.2
lookupHeader :: (MonadReader env m, HasWaiRequest env) => CI S8.ByteString -> m (Maybe S8.ByteString)
lookupHeader = liftM listToMaybe . lookupHeaders

-- | Lookup a request header.
--
-- Since 1.2.2
lookupHeaders :: (MonadReader env m, HasWaiRequest env) => CI S8.ByteString -> m [S8.ByteString]
lookupHeaders key = asks (lookup' key . requestHeaders . view waiRequest)

-- | Lookup basic authentication data from __Authorization__ header of
-- request. Returns user name and password
--
-- Since 1.4.9
lookupBasicAuth :: (MonadReader env m, HasWaiRequest env) => m (Maybe (Text, Text))
lookupBasicAuth = fmap (>>= getBA)
                  (lookupHeader "Authorization")
  where
    getBA bs = (\(x, y) -> ( decodeUtf8With lenientDecode x
                          , decodeUtf8With lenientDecode y))
               <$> extractBasicAuth bs

-- | Lookup bearer authentication datafrom __Authorization__ header of
-- request. Returns bearer token value
--
-- Since 1.4.9
lookupBearerAuth :: (MonadReader env m, HasWaiRequest env) => m (Maybe Text)
lookupBearerAuth = fmap (>>= getBR)
                   (lookupHeader "Authorization")
  where
    getBR bs = decodeUtf8With lenientDecode
               <$> extractBearerAuth bs


-- | Lookup for GET parameters.
lookupGetParams :: (MonadReader env m, HasRichRequest env) => Text -> m [Text]
lookupGetParams pn = asks (lookup' pn . rrGetParams . view richRequest)

-- | Lookup for GET parameters.
lookupGetParam :: (MonadReader env m, HasRichRequest env) => Text -> m (Maybe Text)
lookupGetParam = liftM listToMaybe . lookupGetParams

-- | Lookup for cookie data.
lookupCookie :: (MonadReader env m, HasRichRequest env) => Text -> m (Maybe Text)
lookupCookie = liftM listToMaybe . lookupCookies

-- | Lookup for cookie data.
lookupCookies :: (MonadReader env m, HasRichRequest env) => Text -> m [Text]
lookupCookies pn = asks (lookup' pn . rrCookies . view richRequest)

msgKey :: Text
msgKey = "_MSG"

-- | Sets a message in the user's session.
--
-- See 'getMessage'.
setMessage :: (MonadIO m, MonadReader env m, HasResponseStateRef env) => Html -> m ()
setMessage = setSession msgKey . TL.toStrict . RenderText.renderHtml

-- | Set a variable in the user's session.
--
-- The session is handled by the clientsession package: it sets an encrypted
-- and hashed cookie on the client. This ensures that all data is secure and
-- not tampered with.
setSession :: (MonadIO m, MonadReader env m, HasResponseStateRef env)
           => Text -- ^ key
           -> Text -- ^ value
           -> m ()
setSession k = setSessionBS k . encodeUtf8

-- | Same as 'setSession', but uses binary data for the value.
setSessionBS :: (MonadIO m, MonadReader env m, HasResponseStateRef env)
             => Text
             -> S8.ByteString
             -> m ()
setSessionBS k = modifyResponseState . modSession . Map.insert k

-- | Unsets a session variable. See 'setSession'.
deleteSession :: (MonadIO m, MonadReader env m, HasResponseStateRef env) => Text -> m ()
deleteSession = modifyResponseState . modSession . Map.delete

-- | Clear all session variables.
--
-- Since: 1.0.1
clearSession :: (MonadIO m, MonadReader env m, HasResponseStateRef env) => m ()
clearSession = modifyResponseState $ \x -> x { rsSession = Map.empty }

modSession :: (SessionMap -> SessionMap) -> ResponseState -> ResponseState
modSession f x = x { rsSession = f $ rsSession x }

-- | Internal use only, not to be confused with 'addHeader'.
addHeaderInternal :: (MonadReader env m, MonadIO m, HasResponseStateRef env) => Header -> m ()
addHeaderInternal h = modifyResponseState $ \rs -> rs { rsHeaders = rsHeaders rs . (h:) }

------- Headers
-- | Set the cookie on the client.

setCookie :: (MonadReader env m, MonadIO m, HasResponseStateRef env) => SetCookie -> m ()
setCookie = addHeaderInternal . AddCookie

-- | Helper function for setCookieExpires value
getExpires :: MonadIO m
           => Int -- ^ minutes
           -> m UTCTime
getExpires m = do
    now <- liftIO getCurrentTime
    return $ fromIntegral (m * 60) `addUTCTime` now


-- | Unset the cookie on the client.
--
-- Note: although the value used for key and path is 'Text', you should only
-- use ASCII values to be HTTP compliant.
deleteCookie :: (MonadReader env m, MonadIO m, HasResponseStateRef env)
             => Text -- ^ key
             -> Text -- ^ path
             -> m ()
deleteCookie a = addHeaderInternal . DeleteCookie (encodeUtf8 a) . encodeUtf8


-- | Set the language in the user session. Will show up in 'languages' on the
-- next request.
setLanguage :: (MonadIO m, MonadReader env m, HasResponseStateRef env) => Text -> m ()
setLanguage = setSession langKey

-- | Set an arbitrary response header.
--
-- Note that, while the data type used here is 'Text', you must provide only
-- ASCII value to be HTTP compliant.
--
-- Since 1.2.0
addHeader :: (MonadReader env m, MonadIO m, HasResponseStateRef env) => Text -> Text -> m ()
addHeader a = addHeaderInternal . Header (encodeUtf8 a) . encodeUtf8

-- | Set the Cache-Control header to indicate this response should be cached
-- for the given number of seconds.
cacheSeconds :: (MonadReader env m, MonadIO m, HasResponseStateRef env) => Int -> m ()
cacheSeconds i = addHeader "Cache-Control" $ T.concat
    [ "max-age="
    , T.pack $ show i
    , ", public"
    ]

{- FIXME
-- | Set the Expires header to some date in 2037. In other words, this content
-- is never (realistically) expired.
neverExpires :: MonadReader env m => m ()
neverExpires = do
    addHeader "Expires" . rheMaxExpires =<< askHandlerEnv
    cacheSeconds oneYear
  where
    oneYear :: Int
    oneYear = 60 * 60 * 24 * 365
-}

-- | Set an Expires header in the past, meaning this content should not be
-- cached.
alreadyExpired :: (MonadReader env m, MonadIO m, HasResponseStateRef env) => m ()
alreadyExpired = addHeader "Expires" "Thu, 01 Jan 1970 05:05:05 GMT"

-- | Set an Expires header to the given date.
expiresAt :: (MonadReader env m, MonadIO m, HasResponseStateRef env) => UTCTime -> m ()
expiresAt = addHeader "Expires" . formatRFC1123

{- FIXME
-- | Check the if-none-match header and, if it matches the given value, return
-- a 304 not modified response. Otherwise, set the etag header to the given
-- value.
--
-- Note that it is the responsibility of the caller to ensure that the provided
-- value is a value etag value, no sanity checking is performed by this
-- function.
--
-- Since 1.4.4
setEtag :: MonadReader env m => Text -> m ()
setEtag etag = do
    mmatch <- lookupHeader "if-none-match"
    let matches = maybe [] parseMatch mmatch
    if encodeUtf8 etag `elem` matches
        then notModified
        else addHeader "etag" $ T.concat ["\"", etag, "\""]
-}

-- | Parse an if-none-match field according to the spec. Does not parsing on
-- weak matches, which are not supported by setEtag.
parseMatch :: S.ByteString -> [S.ByteString]
parseMatch =
    map clean . S.split W8._comma
  where
    clean = stripQuotes . fst . S.spanEnd W8.isSpace . S.dropWhile W8.isSpace

    stripQuotes bs
        | S.length bs >= 2 && S.head bs == W8._quotedbl && S.last bs == W8._quotedbl
            = S.init $ S.tail bs
        | otherwise = bs
