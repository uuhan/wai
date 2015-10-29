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
import qualified Data.ByteString.Char8         as S8

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
