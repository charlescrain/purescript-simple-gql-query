module Simple.Graphql.Types 
  ( GraphqlBody(..)
  , GraphQlQuery(..)
  , GraphQlQueryResponse(..)
  , GraphQlQueryResponseError(..)
  , Promise(..)
  , fromAff
  -- , Address
  ) where


import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Promise as Promise
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import HasJSRep (class HasJSRep)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Core.Signatures (Address)
import OhYes (class HasTSRep, toTSRep)
import Simple.Graphql.Errors (RelayerError, handleError)
import Type.Proxy (Proxy(..))

-------------------------------------------------------------------------------
-- | GraphqlBody
-------------------------------------------------------------------------------
type GraphqlBody = {
  query :: String
}

-------------------------------------------------------------------------------
-- | GraphQlQuery
-------------------------------------------------------------------------------
data GraphQlQuery a = GraphQlQuery GraphqlBody (Proxy a)

derive instance genericGraphQlQuery :: Generic (GraphQlQuery a) _

instance showGraphQlQuery :: (Show a) => Show (GraphQlQuery a) where
  show = genericShow

instance eqGraphQlQuery :: (Eq a) => Eq (GraphQlQuery a) where
  eq = genericEq

-------------------------------------------------------------------------------
-- | GraphQlQueryResponseError
-------------------------------------------------------------------------------
type GraphQlQueryResponseError =  {
  message :: String,
  locations :: Array {
    line :: Int,
    column :: Int
  },
  path :: Array String
}

-------------------------------------------------------------------------------
-- | GraphQlQueryResponse
-------------------------------------------------------------------------------
type GraphQlQueryResponse a =  {
  data :: Maybe a,
  errors :: Maybe (Array GraphQlQueryResponseError)
}

-------------------------------------------------------------------------------
-- | Promise
-------------------------------------------------------------------------------
-- | Wrapper for Promise.Promise type for creating typescript types
newtype Promise a = Promise (Promise.Promise a)

instance hasJSRepPromise :: (HasJSRep a) => HasJSRep (Promise a)
instance hasTSRepPromise :: (HasTSRep a) => HasTSRep (Promise a) where
  toTSRep _ = "Promise<" <> a <> ">"  
    where
      a = toTSRep (Proxy :: Proxy a)

fromAff :: forall a. Aff a -> Effect (Promise a)
fromAff = map Promise <<< Promise.fromAff
