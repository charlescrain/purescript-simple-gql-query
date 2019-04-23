module Simple.Graphql.Http
  ( get
  , post
  , patch
  , delete
  ) where

import Prelude

import Affjax (URL)
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadThrow, try)
import Data.Array (catMaybes)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Effect.Aff (throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Simple.Graphql.Errors (HttpRequestError(..))
import Simple.JSON as JSON

-------------------------------------------------------------------------------
-- | mkHttpRequest
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's post. Takes an Auth2 token and Handles HttpConnectionError
mkHttpRequest 
  :: forall a b m. 
     JSON.WriteForeign a 
  => JSON.ReadForeign b
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => Method
  -> Maybe String
  -> URL 
  -> Maybe a 
  -> m b
mkHttpRequest method mAuth url mbody = do
  eres <- liftAff (try $ AX.request req)
  case eres of
    Left err -> throwError <<< HttpConnectionError <<< show $ err
    Right res -> either throwError pure $ decodeWithError (res)
  where
    contentType = maybe Nothing (const (Just <<< ContentType $ MediaType "application/json")) mbody
    content = maybe Nothing (Just <<< RequestBody.string <<< JSON.writeJSON) mbody
    authheader = maybe 
      Nothing 
      (\token -> (Just $ RequestHeader "authorization" ("Bearer " <> token))) mAuth
    req = AX.defaultRequest { url = url
                            , headers = catMaybes [contentType, authheader]
                            , method = Left method
                            , content = content
                            , responseFormat = ResponseFormat.string
                            }
-------------------------------------------------------------------------------
-- | post
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's post. Takes an Auth2 token and Handles HttpConnectionError
post 
  :: forall a b m. 
     JSON.WriteForeign a 
  => JSON.ReadForeign b
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => Maybe String
  -> URL 
  -> Maybe a 
  -> m b
post mAuth url mbody = mkHttpRequest POST mAuth url mbody

-------------------------------------------------------------------------------
-- | get
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's get. Takes an Auth2 token and Handles HttpConnectionError
get 
  :: forall a m. 
     JSON.ReadForeign a
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => Maybe String
  -> URL 
  -> m a
get mAuth url = mkHttpRequest GET mAuth url (Nothing :: Maybe {})

-------------------------------------------------------------------------------
-- | patch
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's patch. Takes an Auth2 token and Handles HttpConnectionError
patch 
  :: forall a b m. 
     JSON.WriteForeign a 
  => JSON.ReadForeign b
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => Maybe String
  -> URL 
  -> Maybe a 
  -> m b
patch mAuth url mbody = mkHttpRequest PATCH mAuth url mbody

-------------------------------------------------------------------------------
-- | delete
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's delete. Takes an Auth2 token and Handles HttpConnectionError
delete 
  :: forall a b m. 
     JSON.WriteForeign a 
  => JSON.ReadForeign b
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => Maybe String
  -> URL 
  -> Maybe a 
  -> m b
delete mAuth url mbody = mkHttpRequest DELETE mAuth url mbody

-------------------------------------------------------------------------------
-- | decodeWithError
-------------------------------------------------------------------------------
-- | Decodes the body for the affjax response. 
decodeWithError 
  :: forall a.
     JSON.ReadForeign a
  => AX.Response (Either AX.ResponseFormatError String)
  -> Either HttpRequestError a
decodeWithError res = case res.body of
    Left err -> Left <<< HttpResponseFormatError $ AX.printResponseFormatError err
    Right bodyStr | statusOk res.status -> case JSON.readJSON bodyStr of
                        Left err -> Left (InvalidJsonBody ("Error: " <> show err <> ": JsonBody" <> bodyStr))
                        Right obj -> Right obj
                  | otherwise -> Left (HttpError res.status (res.statusText <> " : " <> bodyStr))

-------------------------------------------------------------------------------
-- | statusOk
-------------------------------------------------------------------------------
-- | Returns true if the status code is in the valid ranges.
statusOk :: StatusCode -> Boolean
statusOk (StatusCode n) = n >= 200 && n < 300

-------------------------------------------------------------------------------
-- | wrapDoubleQuotes
-------------------------------------------------------------------------------
-- | wrap the string in escaped double quotes, useful for strings in GraphQl
--   queries.
wrapDoubleQuotes :: String -> String
wrapDoubleQuotes str = "\"" <> str <> "\""