module Simple.Graphql.Query
  ( runQuery
  ) where

import Prelude

import Affjax (URL)
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadThrow, try)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Effect.Aff (throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Network.Ethereum.Core.HexString (HexString, mkHexString, unHex)
import Network.Ethereum.Core.Signatures (Address, mkAddress, unAddress)
import Simple.JSON as JSON

import Simple.Graphql.Errors (GraphqlQueryError(..))
import Simple.Graphql.Types (GraphQlQuery(..), GraphQlQueryResponse)

-------------------------------------------------------------------------------
-- | runQuery
-------------------------------------------------------------------------------
-- | General function for querying a graphql api.
runQuery
  :: forall a m. 
     JSON.ReadForeign a
  => MonadAff m 
  => MonadThrow GraphqlQueryError m
  => URL
  -> Maybe String
  -> GraphQlQuery a 
  -> m (GraphQlQueryResponse a)
runQuery url mauth (GraphQlQuery gqlBody _) = post mauth url gqlBody

-------------------------------------------------------------------------------
-- | post
-------------------------------------------------------------------------------
-- | Wrapper function for affjax's post. Takes an Auth2 token and Handles HttpConnectionError
post 
  :: forall a b m. 
     JSON.WriteForeign a 
  => JSON.ReadForeign b
  => MonadAff m 
  => MonadThrow GraphqlQueryError m
  => Maybe String
  -> URL 
  -> a 
  -> m b
post mAuth url body = do
  eres <- liftAff (try $ AX.request req)
  case eres of
    Left err -> throwError <<< HttpConnectionError <<< show $ err
    Right res -> either throwError pure $ decodeWithError (res)
  where
    contentType = ContentType $ MediaType "application/json"
    content = Just <<< RequestBody.string <<< JSON.writeJSON $ body
    authheader = case mAuth of
      Nothing -> []
      Just token -> [ RequestHeader "authorization" ("Bearer " <> token) ]
    req = AX.defaultRequest { url = url
                            , headers = [contentType] <> authheader
                            , method = Left POST
                            , content = content
                            , responseFormat = ResponseFormat.string
                            }

-------------------------------------------------------------------------------
-- | decodeWithError
-------------------------------------------------------------------------------
-- | Decodes the body for the affjax response. 
decodeWithError 
  :: forall a.
     JSON.ReadForeign a
  => AX.Response (Either AX.ResponseFormatError String)
  -> Either GraphqlQueryError a
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

-------------------------------------------------------------------------------
-- | stringToAddress
-------------------------------------------------------------------------------
-- | Util function to turn a string into an Ethereum Address. Throws on failure.
stringToAddress :: String -> Either GraphqlQueryError Address
stringToAddress address =   
  let mAddress  = mkAddress =<< mkHexString address
  in maybe (Left $ NotValidEthereumAddress address) Right mAddress

-------------------------------------------------------------------------------
-- | hexStringToString
-------------------------------------------------------------------------------
-- | Util function to turn a HexString into a string. 
hexStringToString :: HexString -> String
hexStringToString = ((<>) "0x"  ) <<< unHex
-------------------------------------------------------------------------------
-- | addressToString
-------------------------------------------------------------------------------
-- | Util function to turn an Address into a String.
addressToString :: Address -> String
addressToString =  hexStringToString <<< unAddress
