module Simple.Graphql.Errors 
  ( HttpRequestError(..)
  , handleError
  ) where


import Prelude

import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Error, error)


-------------------------------------------------------------------------------
-- | Errors
-------------------------------------------------------------------------------
-- | All possible errors within the relayer
data HttpRequestError = InvalidJsonBody String
                  | HttpError StatusCode String
                  | HttpResponseFormatError String
                  | HttpConnectionError String

derive instance eqHttpRequestError :: Eq HttpRequestError
derive instance genericHttpRequestError :: Generic HttpRequestError _
instance showInsertHttpRequestError :: Show HttpRequestError where
  show = genericShow

-------------------------------------------------------------------------------
-- | handleError
-------------------------------------------------------------------------------
-- | General error handler for the HttpRequestError. Maps from HttpRequestError to 
--   Error.
handleError :: forall a m. (MonadError Error m) => HttpRequestError -> m a
handleError (HttpError (StatusCode sc) err) = throwError <<< error $ "Http Error, status code: " <> show sc <> ", Error: " <> err
handleError (HttpResponseFormatError err) = throwError <<< error $ "Failed to format http repsonse: " <> err
handleError (HttpConnectionError err) = throwError <<< error $ "Failed to connect to remote host: " <> err
handleError (InvalidJsonBody err) = throwError <<< error $ "Failed to parse Json body: " <> err

