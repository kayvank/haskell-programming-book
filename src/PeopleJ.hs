{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
module PeopleJ where

{-
data Value =
  Object (HashMap Text Value)
  | Array(Bector Value=)
  | String Text
  | Numeric Scientific
  | Bool Bool
  | Null
-}
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))

data Person = Person
  { name :: String,
    age :: Int,
    occupation :: Occupation
  }
  deriving (Show)

data Occupation = Occupation
  { title :: String,
    tenure :: Int,
    salary :: Int
  }
  deriving (Show)

{-
in genreal we want to stick our data into object. an object will have a
series of Pairs. A Pair is the Data.Aeson representation of a key-value pair, and it consist of
Text and a Value.Next we combine all these paris into a JSON Object by using the object function.
-}
instance ToJSON Occupation where
  -- toJSON :: Occupation -> Value
  toJSON occupation =
    object
      [ "title" .= toJSON (title occupation),
        "tenure" .= toJSON (tenure occupation),
        "salary" .= toJSON (salary occupation)
      ]

instance ToJSON Person where
  -- toJSON :: Person -> Value
  toJSON person =
    object
      [ "name" .= toJSON (name person),
        "age" .= toJSON (age person),
        "occupation" .= toJSON (occupation person)
      ]

instance FromJSON Occupation where
  parseJSON = withObject "Occupation" $ \o -> do
    title_ <- o .: "title"
    tenure_ <- o .: "tenure"
    salary_ <- o .: "salary"
    return $ Occupation title_ tenure_ salary_

instance FromJSON Person where
  parseJSON = withObject "Person" $ \o -> do
    name_ <- o .: "name"
    age_ <- o .: "age"
    occupation_ <- o .: "occupation"
    return $ Person name_ age_ occupation_
