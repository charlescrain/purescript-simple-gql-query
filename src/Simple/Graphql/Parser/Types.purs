module Simple.Graphql.Parser.Types where

import Data.Maybe (Maybe)

type Schema = {
  types :: Array Type,
  queryType :: Type,
  mutationType :: Type,
  subscriptionType :: Type,
  directives :: Array Directive
}

data Type = Scalar ScalarType
          | Object ObjectType
          | Interface InterfaceType
          | Union UnionType
          | Enum EnumType
          | InputObject InputObjectType
          | NonNull NonNullType

type ScalarType = {
  name :: Maybe String,
  description :: Maybe String
}

type ObjectType = {
  name :: Maybe String,
  description :: Maybe String,
  -- defaults to False
  fields :: {includeDeprecated :: Maybe Boolean} -> Field,
  interfaces :: Maybe (Array Type)
}

type InterfaceType = {
  name :: Maybe String,
  description :: Maybe String,
  -- defaults to False
  fields :: {includeDeprecated :: Maybe Boolean} -> Field,
  possibleTypes :: Maybe (Array Type)
}

type EnumType = {
  name :: Maybe String,
  description :: Maybe String,
  -- defaults to False
  enumValues :: {includeDeprecated :: Maybe Boolean} -> Array EnumValue
}

type UnionType = {
  name :: Maybe String,
  description :: Maybe String,
  possibleTypes :: Maybe (Array Type)
}

type InputObjectType = {
  name :: Maybe String,
  description :: Maybe String,
  inputFields :: Array InputValue
}

type ListType = {
  name :: Maybe String,
  description :: Maybe String,
  ofType :: Type
}

type NonNullType = {
  name :: Maybe String,
  description :: Maybe String,
  ofType :: Type
}

type Field = {
  name :: String
  , description :: Maybe String
  , args :: Array InputValue
  , type :: Type
  , isDeprecated :: Boolean
  , deprecationReason :: Maybe String
}

type InputValue = {
  name :: String
  , description :: Maybe String
  , type :: Type
  , defaultValue :: Maybe String
}

type EnumValue = {
  name :: String
  , description :: Maybe String
  , isDeprecated :: Boolean
  , deprecationReason :: Maybe String
}

type Directive = {
  name :: String
  , description :: Maybe String
  , locations :: Array DirectiveLocation
  , args :: Array InputValue
}

data DirectiveLocation = DL_Query
                       | DL_Mutation
                       | DL_Subscription
                       | DL_Field
                       | DL_FragmentDefinition
                       | DL_FragmentSpread
                       | DL_InlineFragment
                       | DL_Schema
                       | DL_Scalar
                       | DL_Object
                       | DL_FieldDefinition
                       | DL_ArgumentDefinition
                       | DL_Interface
                       | DL_Union
                       | DL_Enum
                       | DL_EnumValue
                       | DL_InputObject
                       | DL_InputFieldDefinition