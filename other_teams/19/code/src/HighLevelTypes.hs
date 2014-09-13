module HighLevelTypes where

import Prelude hiding (foldr)
import FixedSizeSerializable
import Util
import Control.Applicative
import Control.Exception
import Data.Foldable
import Data.Traversable
import SafeNat
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Serialize
import Data.Word (Word8)
import Data.Bits

type Time = SafeNat32
type Room = SafeNat32

data PersonType = Employee | Guest
  deriving (Eq, Ord, Show, Enum)
derive makeSerialize ''PersonType
instance FixedSizeSerializable PersonType where
  fsSize _ = fsSize (P :: P Bool)
  fsGet = do
    b <- fsGet
    return $ if b then Employee else Guest
  fsPut Employee = fsPut True
  fsPut Guest = fsPut False

data Person n = Person
  { getPersonType :: PersonType
  , getName :: n
  }
  deriving (Eq, Ord, Show)
derive makeSerialize ''Person
instance (FixedSizeSerializable n) => FixedSizeSerializable (Person n) where
  fsSize _ = fsSize (P :: P PersonType) + fsSize (P :: P n)
  fsGet = pure Person <*> fsGet <*> fsGet
  fsPut (Person t n) = fsPut t >> fsPut n
employee :: n -> Person n
employee = Person Employee
guest :: n -> Person n
guest = Person Guest

instance Functor Person where
    fmap f (Person p n) = Person p $ f n
instance Foldable Person where
    foldr f a (Person _ n) = f n a
instance Traversable Person where
    traverse f (Person p n) = (pure $ Person p) <*> f n

mapPersonName :: (a -> b) -> Person a -> Person b
mapPersonName f (Person t n) = Person t $ f n

data Action = Enter | Leave
  deriving (Eq, Ord, Show, Enum)
instance FixedSizeSerializable Action where
  fsSize _ = fsSize (P :: P Bool)
  fsGet = do
    b <- fsGet
    return $ if b then Enter else Leave
  fsPut Enter = fsPut True
  fsPut Leave = fsPut False

data Location = Gallery | Room Room
  deriving (Eq, Ord, Show)
instance FixedSizeSerializable Location where
  fsSize _ = fsSize (P :: P (Maybe Room))
  fsGet = do
    rM <- fsGet
    return $ case rM of
      Nothing -> Gallery
      Just rm -> Room rm
  fsPut Gallery  = fsPut (Nothing :: Maybe Room)
  fsPut (Room rm) = fsPut $ Just rm

data AppendCommand n = AppendCommand 
  { apComTime :: Time
  , apComPerson :: Person n
  , apComAction :: Action
  , apComLocation :: Location
  }
  deriving (Eq, Ord, Show)
instance (FixedSizeSerializable n) => FixedSizeSerializable (AppendCommand n) where
  fsSize _ = fsSize (P :: P Time) + fsSize (P :: P (Person n)) + fsSize (P :: P Action) + fsSize (P :: P Location)
  fsGet = pure AppendCommand <*> fsGet <*> fsGet <*> fsGet <*> fsGet
  fsPut (AppendCommand t p a l) = fsPut t >> fsPut p >> fsPut a >> fsPut l

data SerialAppendCommand = SerialAppendCommand
  { sApComTime :: Time
  , sApName :: SafeNat32
  , sApRoom :: SafeNat32
  , sApBools :: Word8
  } deriving (Eq)
instance FixedSizeSerializable SerialAppendCommand where
  fsSize _ = fsSize (P :: P Time) + 2 * fsSize (P :: P SafeNat32) + fsSize (P :: P Word8)
  fsGet = pure SerialAppendCommand <*> fsGet <*> fsGet <*> fsGet <*> fsGet
  fsPut (SerialAppendCommand t n r b) = fsPut t >> fsPut n >> fsPut r >> fsPut b

setBoolAt :: Word8 -> Bool -> Int -> Word8
setBoolAt w b i = if b then setBit w i else w

toSAPBools :: PersonType -> Bool -> Action -> Word8
toSAPBools e isRoom a =
  let isEmp = case e of
        Employee -> True
        Guest -> False in
  let isEnter = case a of
        Enter -> True
        Leave -> False in
  let encEmp   = setBoolAt 0 isEmp 1 in
  let encRoom  = setBoolAt encEmp isRoom 2 in
  let encEnter = setBoolAt encRoom isEnter 3 in
  encEnter

fromSAPBools :: Word8 -> (PersonType,Bool,Action)
fromSAPBools enc =
  let isEmp = testBit enc 1 in
  let isRoom = testBit enc 2 in
  let isEnter = testBit enc 3 in
  (if isEmp then Employee else Guest, isRoom, if isEnter then Enter else Leave)

toSAppendCommand :: AppendCommand SafeNat32 -> SerialAppendCommand
toSAppendCommand (AppendCommand t p a l) =
  let (isRoom,room) = case l of
        Gallery -> (False,0)
        Room r -> (True,r) in
  SerialAppendCommand t (getName p) room (toSAPBools (getPersonType p) isRoom a)

fromSAppendCommand :: SerialAppendCommand -> AppendCommand SafeNat32
fromSAppendCommand (SerialAppendCommand t n r b) =
  let (pt,isRoom,a) = fromSAPBools b in
  let result = AppendCommand t (Person pt n) a (if isRoom then Room r else Gallery) in
  assert (toSAppendCommand result == (SerialAppendCommand t n r b)) result

instance Functor AppendCommand where
    fmap f (AppendCommand a b c d) = AppendCommand a (fmap f b) c d
instance Foldable AppendCommand where
    foldr f a (AppendCommand _ b _ _) = foldr f a b
instance Traversable AppendCommand where
    traverse f (AppendCommand a b c d) = pure (\ x -> AppendCommand a x c d) <*> traverse f b

appendCommandMapPerson :: (Person a -> Person b) -> AppendCommand a -> AppendCommand b
appendCommandMapPerson f (AppendCommand t p a l) = AppendCommand t (f p) a l
