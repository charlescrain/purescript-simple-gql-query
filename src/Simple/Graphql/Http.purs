module Simple.Graphql.Http
  ( get
  , post
  , patch
  , delete
  , get_
  , post_
  , patch_
  , delete_
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
-- | post
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's post. Takes an Auth2 token and Handles HttpConnectionError
post_ 
  :: forall a m. 
     JSON.WriteForeign a 
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => Maybe String
  -> URL 
  -> Maybe a 
  -> m Unit
post_ mAuth url mbody = mkHttpRequest_ POST mAuth url mbody

-------------------------------------------------------------------------------
-- | get
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's get. Takes an Auth2 token and Handles HttpConnectionError
get_ 
  :: forall m. 
     MonadAff m 
  => MonadThrow HttpRequestError m
  => Maybe String
  -> URL 
  -> m Unit
get_ mAuth url = mkHttpRequest_ GET mAuth url (Nothing :: Maybe {})

-------------------------------------------------------------------------------
-- | patch
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's patch. Takes an Auth2 token and Handles HttpConnectionError
patch_ 
  :: forall a m. 
     JSON.WriteForeign a 
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => Maybe String
  -> URL 
  -> Maybe a 
  -> m Unit
patch_ mAuth url mbody = mkHttpRequest_ PATCH mAuth url mbody

-------------------------------------------------------------------------------
-- | delete
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's delete. Takes an Auth2 token and Handles HttpConnectionError
delete_ 
  :: forall a m. 
     JSON.WriteForeign a 
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => Maybe String
  -> URL 
  -> Maybe a 
  -> m Unit 
delete_ mAuth url mbody = mkHttpRequest_ DELETE mAuth url mbody

-------------------------------------------------------------------------------
-- | mkHttpRequest
-------------------------------------------------------------------------------
-- | Wrapper around general http request. Takes an Auth2 token and Handles HttpConnectionError
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
  eres <- liftAff $ AX.request req
  case eres of
    Left err -> throwError <<< HttpConnectionError <<< AX.printError $ err
    Right res -> if statusOk res.status
        then either throwError pure $ decodeWithError res.body
        else throwError (HttpError res.status res.statusText)
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
-- | mkHttpRequest_
-------------------------------------------------------------------------------
-- | Wrapper for general http request that ignores the response. Takes an Auth2 token and Handles HttpConnectionError
mkHttpRequest_ 
  :: forall a m. 
     JSON.WriteForeign a 
  => MonadAff m 
  => MonadThrow HttpRequestError m
  => Method
  -> Maybe String
  -> URL 
  -> Maybe a 
  -> m Unit
mkHttpRequest_ method mAuth url mbody = do
  eeres <- liftAff (try $ AX.request req)
  case eeres of
    Left err -> throwError <<< HttpConnectionError <<< show $ err
    Right (Left err) -> throwError <<< HttpConnectionError <<< AX.printError $ err
    Right (Right res) ->
     if statusOk res.status
          then pure unit
          else throwError (HttpError res.status res.statusText)
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
                            , responseFormat = ResponseFormat.ignore
                            }


-------------------------------------------------------------------------------
-- | decodeWithError
-------------------------------------------------------------------------------
-- | Decodes the body for the affjax response. 
decodeWithError 
  :: forall a.
     JSON.ReadForeign a
  => String
  -> Either HttpRequestError a
decodeWithError body = case JSON.readJSON body of
   Left err -> Left (InvalidJsonBody ("Error: " <> show err <> ": JsonBody" <> body))
   Right obj -> Right obj

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