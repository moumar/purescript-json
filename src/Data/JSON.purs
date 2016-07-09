module Data.JSON
    ( JValue(..), JObject(..), JArray(..), JParser(..)
    , class FromJSON, parseJSON, fail
    , decode, eitherDecode
    , (.:), (.:?), (.!=)
    , lookup, tryLookup, orElse 

    , class ToJSON, toJSON, encode
    , Pair(..), (.=), pair, object
    ) where

import Prelude (class Ord, class Eq, class Show, Unit, id, map, pure, show, unit, bind, ($), (<$>), (<>), (<*>), (==))

import Data.Map as M
import Data.Set as S
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn4, Fn2, Fn3, runFn4, runFn2, runFn3)
import Data.Identity (Identity(..))
import Data.Int (fromNumber, toNumber)
import Data.Tuple (Tuple(..), snd, fst)
import Data.Traversable (sequence)
import Data.Array (fromFoldable)

type JObject = M.Map String JValue
type JArray  = Array JValue
type JParser = Either String

data JValue
    = JObject JObject
    | JArray  JArray
    | JString String
    | JNumber Number
    | JInt    Int
    | JBool   Boolean
    | JNull

instance showValue :: Show JValue where
    show (JObject m) = "JObject " <> show m
    show (JArray vs) = "JArray "  <> show vs
    show (JString s) = "JString " <> show s
    show (JNumber n) = "JNumber " <> show n
    show (JInt    i) = "JInt "    <> show i
    show (JBool   b) = "JBool "   <> show b
    show JNull       = "JNull"

instance eqValue :: Eq JValue where
    eq (JObject a) (JObject b) = a == b
    eq (JArray  a) (JArray  b) = a == b
    eq (JString a) (JString b) = a == b
    eq (JNumber a) (JNumber b) = a == b
    eq (JInt    a) (JInt    b) = a == b
    eq (JBool   a) (JBool   b) = a == b
    eq JNull       JNull       = true
    eq _          _            = false

--------------------------------------------------------------------------------

class FromJSON a where
    parseJSON :: JValue -> JParser a

eitherDecode :: forall a. (FromJSON a) => String -> Either String a
eitherDecode s = do
    v <- jsonToValue s
    parseJSON v

decode :: forall a. (FromJSON a) => String -> Maybe a
decode s = case eitherDecode s of
    Right a -> Just a
    Left  _ -> Nothing

fail :: forall a. String -> JParser a
fail = Left

instance valueFromJSON :: FromJSON JValue where
    parseJSON = Right

instance boolFromJSON :: FromJSON Boolean where
    parseJSON (JBool b) = Right b
    parseJSON i         = fail $ show i <> " is not Boolean."

instance numberFromJSON :: FromJSON Number where
    parseJSON (JNumber n) = pure n
    parseJSON (JInt    i) = pure $ toNumber i
    parseJSON i           = fail $ show i <> " is not Number."

instance intFromJSON :: FromJSON Int where
    parseJSON (JInt    n) = pure n
    parseJSON (JNumber i) = maybe (fail $ show i <> " is not Int.") pure $ fromNumber i
    parseJSON i           = fail $ show i <> " is not Int."

instance unitFromJSON :: FromJSON Unit where
    parseJSON JNull = pure unit
    parseJSON i     = fail $ show i <> " is not Null."

instance stringFromJSON :: FromJSON String where
    parseJSON (JString s) = pure s
    parseJSON i          = fail $ show i <> " is not String."

instance arrayFromJSON :: (FromJSON a) => FromJSON (Array a) where
    parseJSON (JArray a) = sequence $ parseJSON <$> a
    parseJSON i          = fail $ show i <> " is not [a]."

instance tupleFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Tuple a b) where
    parseJSON (JArray [a,b]) = Tuple <$> parseJSON a <*> parseJSON b
    parseJSON i              = fail $ show i <> " is not (a,b)."

instance eitherFromJSON :: (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON (JObject obj) = case fromFoldable $ M.toList obj of
        [Tuple "Right" r] -> Right <$> parseJSON r
        [Tuple "Left"  l] -> Left  <$> parseJSON l
        _                 -> fail $ show obj <> " is not (Either a b)."
    parseJSON i = fail $ show i <> " is not (Either a b)."

instance maybeFromJSON :: (FromJSON a) => FromJSON (Maybe a) where
    parseJSON a = pure $ case parseJSON a of
        Left  _ -> Nothing
        Right r -> Just r

instance setFromJSON :: (Ord a, FromJSON a) => FromJSON (S.Set a) where
    parseJSON x = S.fromFoldable <$> (parseJSON x :: JParser (Array a))

instance mapFromJSON :: (FromJSON a) => FromJSON (M.Map String a) where
    parseJSON (JObject o) = M.fromFoldable <$> (sequence $ fn <$> M.toList o)
      where
        fn (Tuple k v) = case parseJSON v of
            Right r -> pure (Tuple k r)
            Left  l -> fail l
    parseJSON i = fail $ show i <> " is not (Map String a)."

instance identityFromJSON :: (FromJSON a) => FromJSON (Identity a) where
    parseJSON a = Identity <$> parseJSON a

lookup :: forall a. (FromJSON a) => JObject -> String -> JParser a
lookup obj key = case M.lookup key obj of
    Nothing -> Left $ "key " <> show key <> " not present"
    Just v  -> parseJSON v

infix 4 lookup as .:

tryLookup :: forall a. (FromJSON a) => JObject -> String -> JParser (Maybe a)
tryLookup obj key = case M.lookup key obj of
    Nothing -> pure Nothing
    Just v  -> parseJSON v

infix 4 tryLookup as .:?

orElse :: forall a. JParser (Maybe a) -> a -> JParser a
orElse pmval val = fromMaybe val <$> pmval

infix 3 orElse as .!=

foreign import data JSON :: *

foreign import jsonParseImpl :: forall a. Fn3 (String -> a) (JSON -> a) String a

jsonParse :: String -> Either String JSON
jsonParse s = runFn3 jsonParseImpl Left Right s

type Ctors = { null   :: JValue
             , number :: Number  -> JValue
             , int    :: Int     -> JValue
             , bool   :: Boolean -> JValue
             , string :: String  -> JValue
             , array  :: JArray  -> JValue
             , object :: JObject -> JValue
             }

type Auxes = { left   :: String -> Either String JValue
             , right  :: JValue -> Either String JValue
             , either :: forall c. (String -> c) -> (JValue -> c) -> Either String JValue -> c
             , insert :: String -> JValue -> JObject -> JObject
             , empty  :: JObject
             }

foreign import jsonToValueImpl :: Fn2 Auxes Ctors (JSON -> Either String JValue)

jsonToValue :: String -> Either String JValue
jsonToValue s = runFn3 jsonParseImpl Left (runFn2 jsonToValueImpl auxes ctors) s
  where
    ctors = { null: JNull, number: JNumber, int: JInt, bool: JBool, string: JString, array: JArray, object: JObject }
    auxes = { left: Left, right: Right, either: either, insert: insert', empty: empty' }
    insert' = M.insert :: String -> JValue -> JObject -> JObject
    empty'  = M.empty :: JObject

--------------------------------------------------------------------------------

class ToJSON a where
    toJSON :: a -> JValue

type Pair = Tuple String JValue

pair :: forall a. (ToJSON a) => String -> a -> Pair
pair name value = Tuple name (toJSON value)

infix 5 pair as .=

object :: Array Pair -> JValue
object ps = JObject $ M.fromFoldable $ ps

encode :: forall a. (ToJSON a) => a -> String
encode a = valueToString $ toJSON a

instance boolToJSON :: ToJSON Boolean where
    toJSON = JBool

instance numberToJSON :: ToJSON Number where
    toJSON = JNumber

instance intToJSON :: ToJSON Int where
    toJSON = JInt

instance stringToJSON :: ToJSON String where
    toJSON = JString

instance unitToJSON :: ToJSON Unit where
    toJSON _ = JNull

instance arrayToJSON :: (ToJSON a) => ToJSON (Array a) where
    toJSON a = JArray $ toJSON <$> a

instance eitherToJSON :: (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON (Right r) = object ["Right" .= r]
    toJSON (Left  l) = object ["Left"  .= l]

instance mapToJSON :: (ToJSON a) => ToJSON (M.Map String a) where
    toJSON m = JObject $ map toJSON m

instance identityToJSON :: (ToJSON a) => ToJSON (Identity a) where
    toJSON (Identity a) = toJSON a

instance maybeToJSON :: (ToJSON a) => ToJSON (Maybe a) where
    toJSON Nothing  = JNull
    toJSON (Just a) = toJSON a

instance setToJSON :: (ToJSON a) => ToJSON (S.Set a) where
    toJSON s = toJSON $ fromFoldable s

instance tupleToJSON :: (ToJSON a, ToJSON b) => ToJSON (Tuple a b) where
    toJSON (Tuple a b) = JArray [toJSON a, toJSON b]

instance valueToJSON :: ToJSON JValue where
    toJSON = id

foreign import jsNull :: JSON
foreign import unsafeCoerce :: forall a b. a -> b

foreign import objToHash :: Fn4 (JValue -> JSON)
               (Tuple String JValue -> String)
               (Tuple String JValue -> JValue)
               (Array (Tuple String JValue))
               JSON

valueToJSONImpl :: JValue -> JSON
valueToJSONImpl (JObject o) = runFn4 objToHash valueToJSONImpl fst snd $ fromFoldable $ M.toList o
valueToJSONImpl (JArray  a) = unsafeCoerce $ valueToJSONImpl <$> a
valueToJSONImpl (JString s) = unsafeCoerce s
valueToJSONImpl (JNumber n) = unsafeCoerce n
valueToJSONImpl (JInt    i) = unsafeCoerce i
valueToJSONImpl (JBool   b) = unsafeCoerce b
valueToJSONImpl JNull       = jsNull

foreign import jsonStringify :: JSON -> String

valueToString :: JValue -> String
valueToString v = jsonStringify $ valueToJSONImpl v
