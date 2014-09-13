module HighLevel where

import Prelude hiding ((.))
import Control.Category
import Util
import HighLevelTypes
import Lens
import SafeNat

import Control.Monad
import Control.Monad.State (StateT)
import Data.Map (Map)
import Data.Set (Set)
import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Serialize
import Data.Vector.Serialize ()

import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set

-- INVARIANT: range of roomOccupants is a subset of galleryOccupants
-- i.e. (p in [room n]) -> (p in [gallery])
-- i.e. (not p in [gallery]) -> forall n. (not p in [room n])
-- INVARIANT: each mappint [n -> ps] in room occupants is disjoint
-- i.e. (p in [room n]) -> forall m. m =/= n -> (not p in [room m])
data GallerySummary = GallerySummary
  { getGalleryTime :: Time
  , getGalleryOccupants :: Set (Person SafeNat32)
  , getRoomOccupants :: Map SafeNat32 (Set (Person SafeNat32))
  }
derive makeSerialize ''GallerySummary

data GalleryState = GalleryState
  { getGallerySummary :: GallerySummary
  , getPersonHistory :: Map (Person SafeNat32) [Room]         -- Global (probably) NOT cached because potentially on order num events
  -- list of rooms is in descending chronological order (last room first)
  -- TODO: make this a flag
  , getTotalTimeSpent :: Map (Person SafeNat32) SafeNat32     -- Global YES cached because order linear in total peeps
  , getSimultaneousOccupation :: Set Room                     -- Global NOT being cached because...
  , getPresentDuringInterval1 :: Set (Person SafeNat32)       -- Time Range
  , getPresentDuringInterval2 :: Set (Person SafeNat32)       -- Time Range
  }
-- Lenses {{{
gallerySummaryTimeL :: Lens GallerySummary Time
gallerySummaryTimeL = lens getGalleryTime $ \ g t -> g { getGalleryTime = t }
gallerySummaryOccupantsL :: Lens GallerySummary (Set (Person SafeNat32))
gallerySummaryOccupantsL = lens getGalleryOccupants $ \ g o -> g { getGalleryOccupants = o }
roomSummaryOccupantsL :: Lens GallerySummary (Map SafeNat32 (Set (Person SafeNat32)))
roomSummaryOccupantsL = lens getRoomOccupants $ \ g o -> g { getRoomOccupants = o }

gallerySummaryL :: Lens GalleryState GallerySummary
gallerySummaryL = lens getGallerySummary $ \ g s -> g { getGallerySummary = s }
galleryTimeL :: Lens GalleryState Time
galleryTimeL = lens (getGalleryTime . getGallerySummary) $ \ g t -> 
  set gallerySummaryL (set gallerySummaryTimeL t (getGallerySummary g)) g
galleryOccupantsL :: Lens GalleryState (Set (Person SafeNat32))
galleryOccupantsL = lens (getGalleryOccupants . getGallerySummary) $ \ g o -> 
  set gallerySummaryL (set gallerySummaryOccupantsL o (getGallerySummary g)) g
roomOccupantsL :: Lens GalleryState (Map SafeNat32 (Set (Person SafeNat32)))
roomOccupantsL = lens (getRoomOccupants . getGallerySummary) $ \ g o -> 
  set gallerySummaryL (set roomSummaryOccupantsL o (getGallerySummary g)) g
totalTimeSpentL :: Lens GalleryState (Map (Person SafeNat32) SafeNat32)
totalTimeSpentL = lens getTotalTimeSpent $ \ g t -> g { getTotalTimeSpent = t }
simultaneousOccupationL :: Lens GalleryState (Set Room)
simultaneousOccupationL = lens getSimultaneousOccupation $ \ g so -> g { getSimultaneousOccupation = so }
presentDuringInterval1L :: Lens GalleryState (Set (Person SafeNat32))
presentDuringInterval1L = lens getPresentDuringInterval1 $ \ g p -> g { getPresentDuringInterval1 = p }
presentDuringInterval2L :: Lens GalleryState (Set (Person SafeNat32))
presentDuringInterval2L = lens getPresentDuringInterval2 $ \ g p -> g { getPresentDuringInterval2 = p }
-- personHistoryL' :: Lens GallerySummary (Map (Person SafeNat32) [Room])
-- personHistoryL' = personHistoryL . gallerySummaryL
personHistoryL :: Lens GalleryState (Map (Person SafeNat32) [Room])
personHistoryL = lens getPersonHistory $ \ g h -> g { getPersonHistory = h }

-- }}}

type MetaData = Vector ByteString

data MetaDataInfo = MetaDataInfo
  { metaDataNatToName :: Map SafeNat32 ByteString
  , metaDataNameToNat :: Map ByteString SafeNat32
  , metaDataNextID :: SafeNat32
  }
metaDataNatToNameL :: Lens MetaDataInfo (Map SafeNat32 ByteString)
metaDataNatToNameL = lens metaDataNatToName $ \ i m -> i { metaDataNatToName = m }
metaDataNameToNatL :: Lens MetaDataInfo (Map ByteString SafeNat32)
metaDataNameToNatL = lens metaDataNameToNat $ \ i m -> i { metaDataNameToNat = m }
metaDataNextIDL :: Lens MetaDataInfo SafeNat32
metaDataNextIDL = lens metaDataNextID $ \ i m -> i { metaDataNextID = m }

metaDataAddNew :: ByteString -> MetaDataInfo -> (MetaDataInfo, SafeNat32)
metaDataAddNew bs (MetaDataInfo ntonm nmton next) =
    (MetaDataInfo (Map.insert next bs ntonm)
                  (Map.insert bs next nmton) (next + 1), next)

buildMetaMaps :: MetaData -> MetaDataInfo
buildMetaMaps = Vector.ifoldl' ff $ MetaDataInfo Map.empty Map.empty 0
  where
    ff (MetaDataInfo ntos ston _) i name = MetaDataInfo (Map.insert (snat i) name ntos) (Map.insert name (snat i) ston) (snat i + 1)

-- Tail Block --

data TailBlock = TailBlock
  { tailBlockMeta :: MetaData
  , tailBlockSummary :: GallerySummary
  }
derive makeSerialize ''TailBlock

initialSummary :: GallerySummary
initialSummary = GallerySummary
  { getGalleryTime = 0
  , getGalleryOccupants = Set.empty
  , getRoomOccupants = Map.empty
  }

initialGallery :: GalleryState
initialGallery = GalleryState
  { getGallerySummary = initialSummary
  , getPersonHistory = Map.empty
  , getTotalTimeSpent = Map.empty
  , getSimultaneousOccupation = Set.empty
  , getPresentDuringInterval1 = Set.empty
  , getPresentDuringInterval2 = Set.empty
  }

getGalleryEmployees :: GalleryState -> Set (Person SafeNat32)
getGalleryEmployees = Set.filter ((==) Employee . getPersonType) . getGalleryOccupants . getGallerySummary

getGalleryGuests :: GalleryState -> Set (Person SafeNat32)
getGalleryGuests = Set.filter ((==) Guest . getPersonType) . getGalleryOccupants . getGallerySummary

type AppLogic = StateT GalleryState Maybe

basicApplyEvent ::  AppendCommand SafeNat32 -> AppLogic ()
basicApplyEvent = applyEvent False Nothing Nothing Nothing

-- The entire application logic
applyEvent 
  :: Bool                              -- whether or not to update time spent
  -> Maybe (Set (Person SafeNat32))            -- people to watch for rooms of simultaneous occupation (option -I)
  -> Maybe (SafeNat32, SafeNat32)            -- range to watch people in gallery (option -A and -B)
  -> Maybe (SafeNat32, SafeNat32)            -- range to watch people in gallery (option -B)
  -> AppendCommand SafeNat32                   -- the command to append
  -> AppLogic ()
applyEvent doTimeSpent simulM range1M range2M (AppendCommand etime person action location) = do
  do
    time <- getL galleryTimeL
    galleryOccupants <- getL galleryOccupantsL
    roomOccupants <- getL roomOccupantsL
    -- check that time is increasing
    guard $ etime > time
    -- update time spent for everybody
    when doTimeSpent $ do
      let updateMap = Map.fromList $ map (,etime - time) $ Set.toList galleryOccupants
      modifyL totalTimeSpentL $ Map.unionWith (+) updateMap
    -- move time forward
    putL galleryTimeL etime
    -- check that the requested action is valid and perform it
    let allRoomOccupants = Set.unions $ Map.elems roomOccupants
    case (action, location) of
      (Enter, Gallery) -> do
        guard (not $ person `Set.member` galleryOccupants)
        modifyL galleryOccupantsL $ Set.insert person
      (Leave, Gallery) -> do
        guard (person `Set.member` galleryOccupants)
        guard (not $ person `Set.member` allRoomOccupants)
        modifyL galleryOccupantsL $ Set.delete person
      (Enter, Room rid) -> do
        guard (person `Set.member` galleryOccupants)
        guard (not $ person `Set.member` allRoomOccupants)
        modifyL roomOccupantsL $ Map.insertWith Set.union rid $ Set.singleton person
        modifyL personHistoryL $ Map.insertWith (++) person [rid]
      (Leave, Room rid) -> do
        guard (person `Set.member` Map.findWithDefault Set.empty rid roomOccupants)
        modifyL roomOccupantsL $ Map.adjust (Set.delete person) rid
  do
    -- separated and out of scope of previous variables to make sure we get the
    -- latest values (and don't accidently use the old ones)
    time <- getL galleryTimeL
    galleryEmployees <- liftM getGalleryEmployees State.get
    roomOccupants <- getL roomOccupantsL
    -- check for simultaneous occupation
    whenJust simulM $ \ simul ->
      flip mapM_ (Map.toList roomOccupants) $ uncurry $ \ rid rguests ->
        when (simul `Set.isSubsetOf` rguests) $
          modifyL simultaneousOccupationL $ Set.insert rid
    -- check for _EMPLOYEE_ presence during interval 1
    whenJust range1M $ \ (low, high) -> 
      when (time >= low && time <= high) $
        modifyL presentDuringInterval1L $ Set.union galleryEmployees
    -- check for _EMPLOYEE_ presence during interval 2
    whenJust range2M $ \ (low, high) ->
      when (time >= low && time <= high) $ do
        modifyL presentDuringInterval2L $ Set.union galleryEmployees
