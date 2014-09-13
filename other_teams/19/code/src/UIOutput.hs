module UIOutput where

import HighLevel
import Util
import Lens
import HighLevelTypes
import SafeNat

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Builder (Builder)
import Data.List
import Data.Set (Set)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Map as Map
import qualified Data.Set as Set

type IdentityMapping = Person SafeNat32 -> Maybe ByteString

type Output a = ReaderT IdentityMapping (WriterT Builder Maybe) a

runOutput :: IdentityMapping -> Output () -> Maybe Builder
runOutput im o = execWriterT $ runReaderT o im

lookupName :: Person SafeNat32 -> Output ByteString
lookupName n = do
  im <- ask
  maybeZero $ im n

outS :: String -> Output ()
outS = outBS . BS.pack

outBS :: ByteString -> Output ()
outBS = tell . Builder.lazyByteString

outSNL :: String -> Output ()
outSNL s = outS s >> outS "\n"

openTag :: String -> Output ()
openTag s = outS "<" >> outS s >> outS ">"

closeTag :: String -> Output ()
closeTag s = outS "</" >> outS s >> outS ">"

tag :: String -> Output () -> Output ()
tag s o = openTag s >> o >> closeTag s

tagNL :: String -> Output () -> Output ()
tagNL s o = openTag s >> outS "\n" >> o >> closeTag s >> outS "\n"

nlAfter :: Output () -> Output ()
nlAfter o = o >> outS "\n"

commaSep :: [Output ()] -> Output ()
commaSep = sequence_ . intersperse (outS ",")

printGalleryState :: GalleryState -> Output ()
printGalleryState gs = do
  employees <- liftM sort $ mapM lookupName $ Set.toList $ getGalleryEmployees gs
  guests <- liftM sort $ mapM lookupName $ Set.toList $ getGalleryGuests gs
  nlAfter $ commaSep $ map outBS employees
  nlAfter $ commaSep $ map outBS guests
  forM_ (Map.toList $ (access roomOccupantsL) gs) $ uncurry $ \ rid rguests -> do
    when (not $ Set.null rguests) $ nlAfter $ do
      rguestNames <- liftM sort $ mapM lookupName $ Set.toList rguests
      outS $ show rid
      outS ": "
      commaSep $ map outBS rguestNames

printGalleryStateHTML :: GalleryState -> Output ()
printGalleryStateHTML gs = do
  employees <- liftM sort $ mapM lookupName $ Set.toList $ getGalleryEmployees gs
  guests <- liftM sort $ mapM lookupName $ Set.toList $ getGalleryGuests gs
  tagNL "html" $
    tagNL "body" $ do
      tagNL "table" $ do
        tagNL "tr" $ do
          nlAfter $ tag "th" $ outS "Employee"
          nlAfter $ tag "th" $ outS "Guest"
        tagNL "tr" $ do
          forM_ (zipLongest employees guests) $ \ (eM, gM) -> do
            nlAfter $ tag "td" $ whenJust eM outBS
            nlAfter $ tag "td" $ whenJust gM outBS

      tagNL "table" $ do
        tagNL "tr" $ do
          nlAfter $ tag "th" $ outS "Room ID"
          nlAfter $ tag "th" $ outS "Occupants"
        tagNL "tr" $ do
          forM_ (Map.toList $ (access roomOccupantsL) gs) $ uncurry $ \ rid rguests -> do
            rguestNames <- liftM sort $ mapM lookupName $ Set.toList rguests
            nlAfter $ tag "td" $ outS $ show rid
            nlAfter $ tag "td" $ commaSep $ map outBS rguestNames

printRoomInfo :: Person SafeNat32 -> GalleryState -> Output ()
printRoomInfo p gs = do
  let historyM = Map.lookup p $ access personHistoryL gs
  whenJust historyM $ \ history ->
    commaSep $ map (outS . show) $ reverse history

printRoomInfoHTML :: Person SafeNat32 -> GalleryState -> Output ()
printRoomInfoHTML p gs = do
  let historyM = Map.lookup p $ access personHistoryL gs
  tagNL "html" $
    tagNL "body" $
      tagNL "table" $ do
        tagNL "tr" $
          nlAfter $ tag "th" $ outS "Rooms"
        whenJust historyM $ \ history -> 
          forM_ (reverse history) $ \ rid ->
            tagNL "tr" $ 
              nlAfter $ tag "td" $ outS $ show rid

printRooms :: Set Room -> Output ()
printRooms = nlAfter . commaSep . map (outS . show) . Set.toList

printRoomsHTML :: Set Room -> Output ()
printRoomsHTML rs = do
  tagNL "html" $
    tagNL "body" $
      tagNL "table" $ do
        tagNL "tr" $
          nlAfter $ tag "th" $ outS "Rooms"
        forM_ (Set.toList rs) $ \ rid ->
          tagNL "tr" $
            nlAfter $ tag "td" $ outS $ show rid

printEmployees :: Set (Person SafeNat32) -> Output ()
printEmployees ps = do
  pnames <- liftM sort $ mapM lookupName $ Set.toList ps
  nlAfter $ commaSep $ map outBS pnames

printEmployeesHTML :: Set (Person SafeNat32) => Output ()
printEmployeesHTML ps = do
  pnames <- liftM sort $ mapM lookupName $ Set.toList ps
  tagNL "html" $
    tagNL "body" $
      tagNL "table" $ do
        tagNL "tr" $
          nlAfter $ tag "th" $ outS "Employees"
        forM_ pnames $ \ pname ->
          tagNL "tr" $
            nlAfter $ tag "td" $ outBS pname
