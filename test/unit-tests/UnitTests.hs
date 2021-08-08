module UnitTests (runUnitTests) where

import Prelude hiding(id)
import Test.HUnit.Lang
import Test.HUnit.Base(Counts(..), (@?), (~:), test)
import Test.HUnit.Text (runTestTT)
import NoteService (createNote, getAllNotes, deleteNote, modifyNote, DiskFileStorageConfig(..), Error(..))
import Model (NoteContent(..), StorageId(..), Note(..), NoteUpdate(..))
import System.Directory (removeDirectoryRecursive, createDirectory,doesDirectoryExist, doesFileExist, listDirectory)
import Data.Maybe (fromJust, isJust)
import Data.List ((\\))
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (runExceptT)
import System.Exit (exitSuccess, exitFailure)

runUnitTests :: IO ()
runUnitTests = runTestTTAndExit noteServiceTests

runTestTTAndExit tests = do
  c <- runTestTT tests
  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure

noteServiceTests = test [ "Creating a note should create a new file in storage directory" ~: withEmptyNoteDir createNoteTest
                        , "Getting all notes on an empty storage directory should give an empty list" ~: getEmptyDirTest
                        , "Creating then getting all notes should give back the note" ~: withEmptyNoteDir createManyThenGetTest
                        , "Creating then deleting all notes should give back no note" ~: withEmptyNoteDir createManyThenDeleteAllTest
                        , "Deleting on an empty storage should always be an error" ~: deleteNoteOnEmptyDir
                        , "Modifying an existing note should give back the modified note" ~: withEmptyNoteDir modifyAnExistingNote
                        , "Modifying an non-existing note should give back a NotFoundError" ~: withEmptyNoteDir modifyANonExistingNote
                        , "Modifying an existing note but with wrong current version should give back a NotCurrentVersion error" ~: withEmptyNoteDir modifyWrongCurrentVersion
                        ]

noteDirPath = "target/.sharad/data/notes/"

withEmptyNoteDir = withEmptyDir noteDirPath

createNoteTest = do
    Just storageId <- runMaybeT $ createNote testDiskConfig noteExample
    dirContent <- retriveContentInDir noteDirPath
    noteFilePathFromId storageId `elem` dirContent @? "expected " ++ show dirContent ++ " to contain " ++ show (id storageId ++ ".txt")
    where
        noteExample = NoteContent { title = Just "ExampleNoteTitle", content = "Arbitrary note content" }

getEmptyDirTest = withEmptyNoteDir $ do
    Right notes <- runExceptT $ getAllNotes testDiskConfig
    assertEqual ("Expected [] but got " ++ show notes) [] notes

createManyThenGetTest = do
    maybecreationIds <- mapM (runMaybeT . createNote testDiskConfig) noteExamples
    let creationIds = map fromJust maybecreationIds
    Right notes <- runExceptT $ getAllNotes testDiskConfig
    assertEqual "Their should be one note retrieved" (length creationIds) (length notes)
    assertEqualWithoutOrder "RetrievedNote should have the same id as the created note" creationIds (map noteId notes)
    assertEqualWithoutOrder "RetrievedNote should have the same content as the created note" noteExamples (map noteContent notes)
    where noteExamples = [ NoteContent { title = Just ("ExampleNoteTitle " ++ show int), content = "Arbitrary note content " ++ show int } | int <- [1..5] ]

createManyThenDeleteAllTest = do
    maybecreationIds <- mapM (runMaybeT . createNote testDiskConfig) noteExamples
    let noteIds = map (id . fromJust) maybecreationIds
    results <- mapM (deleteNote testDiskConfig) noteIds
    assertBool "all deletions should be a success" (not (any isJust results))
    dirContent <- retriveContentInDir noteDirPath
    assertEqual "note storage should be empty" [] dirContent
    where noteExamples = [ NoteContent { title = Just ("ExampleNoteTitle " ++ show int), content = "Arbitrary note content " ++ show int } | int <- [1..5] ]

deleteNoteOnEmptyDir = withEmptyNoteDir $ do
    Just toto <- deleteNote testDiskConfig "ArbitraryNoteId"
    assertEqual "The note should not be found" (NotFound "ArbitraryNoteId") toto

modifyAnExistingNote = do
    Just creationId <- runMaybeT $ createNote testDiskConfig noteExample
    Right newcreationId <- runExceptT $ modifyNote testDiskConfig (arbitraryNoteUpdate creationId)
    assertEqual "Updated note id should be the same as original note" (id creationId) (id newcreationId)
    assertNotEqual "Updated note version should be different from original note's version" (version creationId) (version newcreationId)
    where
        noteExample = NoteContent { title = Just "ExampleNoteTitle", content = "Arbitrary note content" }
        arbitraryNoteUpdate creationId = NoteUpdate creationId (NoteContent { title = Just "ModifiedNoteTitle", content = "Modified content too"})

modifyANonExistingNote = do
    Left error <- runExceptT $ modifyNote testDiskConfig (arbitraryNoteUpdate noteId)
    assertEqual "Error should be a NotFound of the requested id" (NotFound "id") error
    where
        arbitraryNoteUpdate storageId = NoteUpdate StorageId { id = "id", version = "" } (NoteContent { title = Just "ModifiedNoteTitle", content = "Modified content too"})

modifyWrongCurrentVersion = do
    Just creationId <- runMaybeT $ createNote testDiskConfig noteExample
    Left error <- runExceptT $ modifyNote testDiskConfig (wrongVersionNoteUpdate creationId)
    assertEqual "Error should be a WrongVersion of the storageId requested" (NotCurrentVersion $ wrongVersionStorageId creationId) error
    where
        noteExample = NoteContent { title = Just "ExampleNoteTitle", content = "Arbitrary note content" }
        wrongVersionStorageId StorageId { id = creationId, version = creationVersion } =
            StorageId { id = creationId, version = creationVersion ++ "make it wrong"}
        wrongVersionNoteUpdate creationStorageId =
            NoteUpdate (wrongVersionStorageId creationStorageId) (NoteContent { title = Just "ModifiedNoteTitle", content = "Modified content too"})

assertEqualWithoutOrder s as bs = do
    assertBool (s ++ "\n\t" ++ show as ++ " should be equal in " ++ show bs) (null (as \\ bs))
    assertBool (s ++ "\n\t" ++ show bs ++ " should be equal in " ++ show as) (null (bs \\ as))

assertNotEqual s a b = assertBool s (a /= b)

assertBool s = assertEqual s True

noteFilePathFromId :: StorageId -> String
noteFilePathFromId storageId = noteDirPath ++ id storageId ++ ".txt"

retriveContentInDir :: FilePath -> IO [FilePath]
retriveContentInDir dirPath = do
    dirFileNames <- listDirectory dirPath
    return $ map (dirPath ++) dirFileNames

hasFile :: [FilePath] -> IO Bool
hasFile dirContent = or <$> mapM doesFileExist dirContent

testDiskConfig :: DiskFileStorageConfig
testDiskConfig = DiskFileStorageConfig { rootPath = noteDirPath }

withEmptyDir dirPath _test = do
    exists <- doesDirectoryExist dirPath
    if exists
        then do
            removeDirectoryRecursive dirPath
            _test
        else do
            _test

