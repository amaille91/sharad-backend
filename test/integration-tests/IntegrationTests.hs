{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IntegrationTests (runIntegrationTests) where

import Prelude hiding (id)
import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.ByteString.UTF8
import           Data.Functor.Identity
import           GHC.Exts
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple
import           Test.Hspec
import           Test.HUnit
import Model

runIntegrationTests :: IO ()
runIntegrationTests = hspec $ do
    describe "first test" $ do
      it "should satisfy the basics, in one session, of the Very First User's needs" $ do
        let
          firstNoteContent = NoteContent { title = Just "First note", content = "First note content" }
          firstNoteNewContent = NoteContent { title = Just "First note new title", content = "This is a new content for the first note" }
        assertServerIsNew
        createNewNote firstNoteContent
        [firstNote] <- assertGetNoteWithContent firstNoteContent
        modifyNotes $ NoteUpdate (storageId firstNote) firstNoteNewContent
        [modifiedNote] <- assertGetNoteWithContent firstNoteNewContent
        deleteCreatedNote $ (id.storageId) modifiedNote
        assertServerIsNew

assertServerIsNew :: Expectation
assertServerIsNew = do
  getResponse :: Response [Note] <- sendRequestWithJSONBody "GET" ()
  assertNoNoteInResponse "Failed to start with an empty server" getResponse

createNewNote :: NoteContent -> Expectation
createNewNote noteToCreate = do
  postResponse :: Response StorageId <- sendRequestWithJSONBody "POST" noteToCreate
  assertStatusCode200 "Failed to create first note" postResponse

assertGetNoteWithContent :: NoteContent -> IO [Note]
assertGetNoteWithContent expectedNoteContent = do
  getResponse <- sendRequestWithJSONBody "GET" ()
  assertNotesWithContentsFound "Failed to retriev created note" [expectedNoteContent] getResponse

modifyNotes :: NoteUpdate -> Expectation
modifyNotes noteupdate = do
  putResponse :: Response StorageId <- sendRequestWithJSONBody "PUT" noteupdate
  assertStatusCode200 "Failed to modify first note" putResponse

deleteCreatedNote :: String -> Expectation
deleteCreatedNote idToDelete = do
  deleteResponse <- parseRequest ("DELETE http://localhost:8081/note/" ++ idToDelete) >>= httpBS
  assertStatusCode200 "Failed to delete modified note" deleteResponse

assertNotesWithContentsFound :: String -> [NoteContent] -> Response [Note] -> IO [Note]
assertNotesWithContentsFound errorPrefix expectedNoteContents response = do
  assertStatusCode200 errorPrefix response
  assertEqual (errorPrefix ++ "\n\tExpected notes with contents:\n" ++ show expectedNoteContents ++ "\nin response") expectedNoteContents  (map noteContent responseNotes)
  return responseNotes
  where
    responseNotes :: [Note]
    responseNotes = getResponseBody response

sendRequestWithJSONBody :: (ToJSON requestType, FromJSON responseType) =>
  ByteString -> requestType -> IO (Response responseType)
sendRequestWithJSONBody method body = httpJSON $
  setRequestBodyJSON body $
    setRequestHeader "Content-Type" ["application/json"] $
      setRequestMethod method "http://localhost:8081/note"

sendRequestSimple :: (ToJSON requestType) => ByteString -> requestType -> IO (Response ByteString)
sendRequestSimple method body = httpBS $
  setRequestBodyJSON body $
    setRequestHeader "Content-Type" ["application/json"] $
      setRequestMethod method "http://localhost:8081/note"

assertNoNoteInResponse errorPrefix response = do
  assertStatusCode200 errorPrefix response
  assertEqual (errorPrefix ++ "Expected no note in response"     ) []  (toList $ getResponseBody response)

assertStatusCode200 :: String -> Response a -> Assertion
assertStatusCode200 errorPrefix response = assertEqual (errorPrefix ++ "Expected 200 response status code") 200 (getResponseStatusCode response)

modifyNote :: NoteContent -> Note -> NoteUpdate
modifyNote newContent previousNote = NoteUpdate { newContent = newContent, targetId = storageId previousNote }

