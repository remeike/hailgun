{-# LANGUAGE CPP #-}
module Mail.Hailgun.Domains
    ( getDomains
    , HailgunDomainResponse(..)
    , HailgunDomain(..)
    ) where

#if __GLASGOW_HASKELL__ < 800
import           Control.Applicative
#endif
import           Control.Monad              (mzero)
import           Data.Aeson
import qualified Data.Text                  as T
import           Mail.Hailgun.Communication
import           Mail.Hailgun.Errors
import           Mail.Hailgun.Internal.Data
import           Mail.Hailgun.MailgunApi
import           Mail.Hailgun.Pagination
import qualified Network.HTTP.Client        as NC
import           Network.HTTP.Client.TLS    (tlsManagerSettings)

-- | Make a request to Mailgun for the domains against your account. This is a paginated request so you must specify
-- the pages of results that you wish to get back.
getDomains
   :: HailgunContext -- ^ The context to operate in which specifies which account to get the domains from.
   -> Page -- ^ The page of results that you wish to see returned.
   -> IO (Either HailgunErrorResponse HailgunDomainResponse) -- ^ The IO response which is either an error or the list of domains.
getDomains context page = do
   request <- getRequest url context (toQueryParams . pageToParams $ page)
#if MIN_VERSION_http_client(0,5,0)
   mgr <- NC.newManager tlsManagerSettings
   response <- NC.httpLbs request mgr
#else
   response <- NC.withManager tlsManagerSettings (NC.httpLbs request)
#endif
   return $ parseResponse response
   where
      url = mailgunApiPrefix ++ "/domains"

data HailgunDomainResponse = HailgunDomainResponse
   { hdrTotalCount :: Integer
   , hdrItems      :: [HailgunDomain]
   }
   deriving (Show)

instance FromJSON HailgunDomainResponse where
   parseJSON (Object v) = HailgunDomainResponse
      <$> v .: T.pack "total_count"
      <*> v .: T.pack "items"
   parseJSON _ = mzero

data HailgunDomain = HailgunDomain
   { domainName         :: T.Text
   , domainSmtpLogin    :: String
   , domainSmtpPassword :: String
   , domainCreatedAt    :: HailgunTime
   , domainWildcard     :: Bool
   , domainSpamAction   :: String -- TODO the domain spam action is probably better specified
   }
   deriving(Show)

instance FromJSON HailgunDomain where
   parseJSON (Object v) = HailgunDomain
      <$> v .: T.pack "name"
      <*> v .: T.pack "smtp_login"
      <*> v .: T.pack "smtp_password"
      <*> v .: T.pack "created_at"
      <*> v .: T.pack "wildcard"
      <*> v .: T.pack "spam_action"
   parseJSON _ = mzero
