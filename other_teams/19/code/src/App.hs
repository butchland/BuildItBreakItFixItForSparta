module App where

import HighLevel
import HighLevelTypes
import MidLevel
import LowLevel
import UIInput
import UIOutput
import Util
import Types
import System.IO
import SafeNat
import Lens

import Control.Monad.State
import System.Exit

import Data.Traversable
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vector

readCommandLogic :: ReadCommand SafeNat32 -> (AppendCommand SafeNat32 -> AppLogic (), GalleryState -> Output ())
readCommandLogic (SummaryC mode) =
  let output = case mode of
        Text -> printGalleryState
        HTML -> printGalleryStateHTML
  in (basicApplyEvent, output)
readCommandLogic (RoomC mode person) =
  let output = case mode of
        Text -> printRoomInfo person
        HTML -> printRoomInfoHTML person
  in (basicApplyEvent, output)
readCommandLogic (TotalTimeC person) =
  let apply = applyEvent True Nothing Nothing Nothing
      output gs = whenJust (Map.lookup person $ getTotalTimeSpent gs) $ outSNL . show
  in (apply, output)
readCommandLogic (SimultaneousC mode people) =
  let apply = applyEvent False (Just $ Set.fromList people) Nothing Nothing
      output gs = case mode of
        Text -> printRooms $ getSimultaneousOccupation gs
        HTML -> printRoomsHTML $ getSimultaneousOccupation gs
  in (apply, output)
readCommandLogic (RangeC mode range) =
  let apply = applyEvent False Nothing (Just range) Nothing
      output gs = case mode of
        Text -> printEmployees $ getPresentDuringInterval1 gs
        HTML -> printEmployeesHTML $ getPresentDuringInterval1 gs
  in (apply, output)
readCommandLogic (RangeDifferenceC mode range1 range2) =
  let apply = applyEvent False Nothing (Just range1) (Just range2)
      output gs = 
        let result = getPresentDuringInterval1 gs Set.\\ getPresentDuringInterval2 gs
        in case mode of
          Text -> printEmployees result
          HTML -> printEmployeesHTML result
  in (apply, output)

convert :: String -> State MetaDataInfo SafeNat32
convert name = do
  let bsname = BS.pack name
  stoi <- getL metaDataNameToNatL
  case Map.lookup bsname stoi of
    Nothing -> do
      i <- nextL metaDataNextIDL
      modifyL metaDataNatToNameL $ Map.insert i bsname
      modifyL metaDataNameToNatL $ Map.insert bsname i
      return i
    Just i -> return i

errSpec :: Err -> (String, Handle)
errSpec e = case e of
  -- thrown by App
  InvalidInput -> ("invalid", stdout)
  InternalError msg -> ("internal error: " ++ msg, stderr)
  -- thrown by MidLevel
  SecurityViolation -> ("security error", stderr)
  IntegrityViolation -> ("integrity violation", stderr)
  CorruptedData -> ("integrity violation", stderr)
  -- thrown by LowLevel
  MasterBlockOverflow _ -> ("internal error: master block overflow", stderr)
  DataBlockOverflow _ _ -> ("internal error: data block overflow", stderr)
  TailBlockOverflowRead _ _ -> ("integrity violation", stderr)
  -- these should not be used by us
  NoMsg -> ("internal error", stderr)
  StrMsg _ -> ("internal error", stderr)

handleErrAndDie :: Err -> IO a
handleErrAndDie e =
  let (msg, h) = errSpec e
  in do
    hPutStrLn h msg
    exitWith $ ExitFailure (-1)

handleErr :: BatchingMode -> Err -> IO ()
handleErr batchMode e = do
  let (msg, h) = errSpec e
  hPutStrLn h msg
  case batchMode of
    Single -> exitWith $ ExitFailure (-1)
    Batch -> return ()

mainLogread :: IO ()
mainLogread = do
  UIReadCommand token filename command <- whenNothing (handleErrAndDie InvalidInput) *$ runInputIO parseReadCommand
  whenLeft (handleErr Single) *$ 
    execMFile filename $
      runMM token $ do
        TailBlock metaBlk summaryBlk <- getMetaAndSummary
        let mmaps = buildMetaMaps metaBlk
        let fetchName name = evalState (convert name) mmaps
        let (apply, output) = readCommandLogic $ readCommandMapPerson (mapPersonName fetchName) command
        finalGallery <- case command of
          SummaryC _ -> 
            return $ initialGallery { getGallerySummary = summaryBlk }
          -- RoomC _ _ ->
          --   return $ initialGallery { getGallerySummary = summaryBlk }
          _ -> do
            galleryAction <- foldChain dataChainInfoL (return ()) $ \ c gs -> gs >> apply c
            liftMFile $ whenNothing (throwMFile InvalidInput) $ execStateT galleryAction initialGallery
        liftIO . Builder.hPutBuilder stdout *$ liftMFile $ whenNothing (throwMFile $ InternalError "output failed") $ runOutput (\ p -> Map.lookup (getName p) $ metaDataNatToName mmaps) $ output finalGallery

mainLogappend :: IO ()
mainLogappend = do
  (batchMode, commandMs) <- whenNothing (handleErrAndDie InvalidInput) *$ parseAppendCommands
  forM_ commandMs $ \ commandM -> do
    maybeMOn commandM (handleErr batchMode InvalidInput) $ \ (UIAppendCommand token filename command) -> do
      whenLeft (handleErr batchMode) *$
        execMFile filename $
          runMM token $ do
            TailBlock metaBlk summaryBlk <- getAndDestroyMetaAndSummary
            let summaryGallery = initialGallery { getGallerySummary = summaryBlk }
            let mmaps = buildMetaMaps metaBlk
            case traverse (\x -> Map.lookup (BS.pack x) (metaDataNameToNat mmaps)) command of
              Nothing -> do
                  -- The name is not in the meta-data table
                  let name = BS.pack $ getName $ apComPerson command
                  let theID = access metaDataNextIDL mmaps
                  let theEvent = fmap (\ _ -> theID) command
                  ((),newSummary) <- liftMFile $ whenNothing (throwMFile InvalidInput) $ runStateT (basicApplyEvent theEvent) summaryGallery
                  -- Now things should work the way they work in the
                  -- success case. The only difference is that we have
                  -- to update the meta-data table in the file
                  appendRecordToChain dataChainInfoL theEvent
                  putMetaAndSummary $ TailBlock (metaBlk `Vector.snoc` name) (getGallerySummary newSummary)
              Just theEvent -> do
                  -- The name is already in the meta-data table
                  ((),newSummary) <- liftMFile $ whenNothing (throwMFile InvalidInput) $ runStateT (basicApplyEvent theEvent) summaryGallery
                  -- TODO: is there some way to not update the metaBlock here?
                  appendRecordToChain dataChainInfoL theEvent
                  putMetaAndSummary $ TailBlock metaBlk (getGallerySummary newSummary)


-- to do this once per batch, find all commands that use the same file, and do them all at once.
