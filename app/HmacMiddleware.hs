{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: rewrite this abomination
-- TODO: move to separate project

module HmacMiddleware
    ( hmacAuth
    , AuthSettings (..)
    , defaultAuthSettings
    , HashAlgorithm
    , SHA1
    ) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)

import           Crypto.Hash
import           Crypto.MAC.HMAC

import           Data.ByteArray             (ByteArrayAccess)
import qualified Data.ByteArray             as BA
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as BSB16
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as SL8
import           Data.CaseInsensitive       (CI)
import           Data.IORef
import           Data.Word8                 (_equal)

import qualified Network.HTTP.Types         as Http
import           Network.Wai

-- ----------------------------------------------

data AuthSettings hash = AuthSettings
    { authHeader :: CI ByteString                 -- ^ Name of the header containing the HMAC-signature
    , authSecret :: ByteString                    -- ^ Secret for HMAC calculation
    , authCheck  :: Request -> IO Bool            -- ^ If this request should be checked or just pass through
    , authFail   :: AuthException -> Application  -- ^ What to do if authentication fails
    , authHash   :: hash                          -- ^ HMAC signing algorithm
    }


-- | Auth failures
data AuthException
  = MissingHeader
  | MalformedSignature
  | SignatureMismatch
  deriving Show

-- ----------------------------------------------

-- | HMAC authentication middleware
hmacAuth :: forall hash . HashAlgorithm hash
  => AuthSettings hash
  -> Middleware
hmacAuth cfg app req resp = do
  shouldBeChecked <- authCheck cfg req
  mayPass <-
    if shouldBeChecked
    then check
    else return $ Right (req, resp)
  case mayPass of
    Left e              -> authFail cfg e req resp
    Right (req', resp') -> app req' resp'

  where
  check =
    case lookup "x-hub-signature" $ requestHeaders req of
      Nothing -> return $ Left MissingHeader
      Just headerVal -> -- XXX: eww
        if hash == "sha1" && BS.length sigWithEqSign == 40+1
        then checkSignature sig
        else return $ Left MalformedSignature

        where
        (hash, sigWithEqSign) = BS.span (_equal /=) headerVal
        sig = BS.tail sigWithEqSign
        checkSignature sig = do
          (req', reqSig) <- calculateSignature cfg req
          if reqSig == sig
            then return $ Right (req', resp)
            else return $ Left SignatureMismatch

-- ----------------------------------------------

defaultAuthSettings :: ByteString -> AuthSettings SHA1
defaultAuthSettings secret = AuthSettings
  { authHeader = "x-hub-signature"
  , authSecret = secret
  , authFail   = \_exc _req f -> f $ responseLBS Http.status403 [] ""
  , authCheck  = \_req -> return True
  , authHash   = SHA1 -- XXX: already covered
  }

-- ----------------------------------------------

calculateSignature :: forall m hash . ( MonadIO m, HashAlgorithm hash )
  => AuthSettings hash -> Request -> m (Request, ByteString)
calculateSignature cfg req = do
  (req', body) <- liftIO $ getRequestBody req
  let bodyStrict = SL8.toStrict . SL8.fromChunks $ body
  let HMAC hashed = hmac (authSecret cfg) bodyStrict :: HMAC hash
  return (req', toHexByteString hashed) -- digestToHexByteString hashed with cryptohash (2 less deps...)
  where
  -- taken from: wai-extra/src/Network/Wai/Middleware/RequestLogger.hs
  getRequestBody :: Request -> IO (Request, [BS8.ByteString])
  getRequestBody req = do
    let loop front = do
           bs <- requestBody req
           if BS8.null bs
               then return $ front []
               else loop $ front . (bs:)
    body <- loop id
    ichunks <- newIORef body
    let rbody = atomicModifyIORef ichunks $ \chunks ->
           case chunks of
               [] -> ([], BS8.empty)
               x:y -> (y, x)
    let req' = req { requestBody = rbody }
    return (req', body)

  toHexByteString :: ByteArrayAccess a => a -> ByteString
  toHexByteString = BSB16.encode . BA.convert
