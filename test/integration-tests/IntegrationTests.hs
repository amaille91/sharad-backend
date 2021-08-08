{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
  describe "Integration Tests" $ do
    it "should satisfy the basics, in one session, of the Very First User's needs, note-wise" $ do
      let
        firstNoteContent = NoteContent { title = Just "First note", content = "First note content" }
        firstNoteNewContent = NoteContent { title = Just "First note new title", content = "This is a new content for the first note" }
      assertServerIsNew
      createNewContent firstNoteContent
      [firstNote :: Note] <- assertGetWithContent firstNoteContent
      modify $ NoteUpdate (storageId firstNote) firstNoteNewContent
      [modifiedNote] <- assertGetWithContent firstNoteNewContent
      deleteNote $ (id.noteId) modifiedNote
      assertServerIsNew
    it "should satisfy the basics, in one session of the Very Firsr User's needs, checklist-wise" $ do
      createNewContent firstChecklistContent
      [firstChecklist :: Checklist] <- assertGetWithContent firstChecklistContent
      modify $ ChecklistUpdate (storageId firstChecklist) firstChecklistNewContent
      [modifiedChecklist :: Checklist] <- assertGetWithContent  firstChecklistNewContent
      deleteChecklist $ (id.storageId) modifiedChecklist
        where
        firstChecklistContent    = ChecklistContent { name = "First checklist"
                                                     , items = [ ChecklistItem { label = "First item label unchecked", checked = False }
                                                               , ChecklistItem { label = "Second item label checked", checked = True }
                                                               ]
                                                     }
        firstChecklistNewContent = ChecklistContent { name = "new checklist"
                                                    , items = [ ChecklistItem { label = "Third item label checked", checked = True }
                                                              , ChecklistItem { label = "Fourth item label checked", checked = True }
                                                              ]
                                                    }

assertServerIsNew :: Expectation
assertServerIsNew = do
  getResponse :: Response [Note] <- sendRequestWithJSONBody "GET" ()
  assertNoNoteInResponse "Failed to start with an empty server" getResponse

createNewContent :: (Content contentType) => contentType -> Expectation
createNewContent content = do
  postResponse :: Response StorageId <- sendRequestWithJSONBody "POST" content
  assertStatusCode200 ("Failed to create item" ++ show content) postResponse

assertGetWithContent :: (GettableContent itemType) => ContentType itemType -> IO [itemType]
assertGetWithContent expectedContent = do
  getResponse <- sendRequestWithJSONBody "GET" ()
  assertWithFoundContent ("Failed to retrieve created content" ++ show expectedContent) [expectedContent] getResponse

modify :: Update updateType => updateType -> Expectation
modify update = do
  putResponse :: Response StorageId <- sendRequestWithJSONBody "PUT" update
  assertStatusCode200 ("Failed to apply modification " ++ show update) putResponse

deleteNote = deleteItem "note/"
deleteChecklist = deleteItem "checklist/"

deleteItem :: String -> String -> Expectation
deleteItem rootItemPath idToDelete = do
  deleteResponse <- parseRequest ("DELETE http://localhost:8081/" ++ rootItemPath ++ idToDelete) >>= httpBS
  assertStatusCode200 ("Failed to delete item" ++ show idToDelete) deleteResponse

assertWithFoundContent :: GettableContent itemType => String -> [ContentType itemType] -> Response [itemType] -> IO [itemType]
assertWithFoundContent errorPrefix expectedContents response = do
  assertStatusCode200 errorPrefix response
  assertEqual (errorPrefix ++ "\n\tExpected notes with contents:\n" ++ show expectedContents ++ "\nin response") expectedContents  (map getContent responseItems)
  return responseItems
  where
    responseItems = getResponseBody response

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
modifyNote newContent previousNote = NoteUpdate (noteId previousNote) newContent


