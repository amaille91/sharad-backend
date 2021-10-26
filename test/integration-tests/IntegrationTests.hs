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
        firstNoteContent = NoteContent { title = Just "First note", noteContent = "First note content" }
        firstNoteNewContent = NoteContent { title = Just "First note new title", noteContent = "This is a new content for the first note" }
      assertServerIsNew
      createNewContent firstNoteContent
      [firstNote :: Identifiable NoteContent] <- assertGetWithContent firstNoteContent
      modify $ Identifiable (storageId firstNote) firstNoteNewContent
      [modifiedNote] <- assertGetWithContent firstNoteNewContent
      deleteNote $ (id.storageId) modifiedNote
      assertServerIsNew
    it "should satisfy the basics, in one session of the Very Firsr User's needs, checklist-wise" $ do
      createNewContent firstChecklistContent
      [firstChecklist :: Identifiable ChecklistContent] <- assertGetWithContent firstChecklistContent
      modify $ Identifiable (storageId firstChecklist) firstChecklistNewContent
      [modifiedChecklist :: Identifiable ChecklistContent] <- assertGetWithContent  firstChecklistNewContent
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
  getResponse :: Response [Identifiable NoteContent] <- sendRequestWithJSONBody "GET" ()
  assertNoNoteInResponse "Failed to start with an empty server" getResponse

createNewContent :: (Content contentType) => contentType -> Expectation
createNewContent content = do
  postResponse :: Response StorageId <- sendRequestWithJSONBody "POST" content
  assertStatusCode200 ("Failed to create item" ++ show content) postResponse

assertGetWithContent :: Content a => a -> IO [Identifiable a]
assertGetWithContent expectedContent = do
  getResponse <- sendRequestWithJSONBody "GET" ()
  assertWithFoundContent ("Failed to retrieve created content " ++ show expectedContent) [expectedContent] getResponse

modify :: Content a => Identifiable a -> Expectation
modify update = do
  putResponse :: Response StorageId <- sendRequestWithJSONBody "PUT" update
  assertStatusCode200 ("Failed to apply modification " ++ show update) putResponse

deleteNote = deleteItem "note/"
deleteChecklist = deleteItem "checklist/"

deleteItem :: String -> String -> Expectation
deleteItem rootItemPath idToDelete = do
  deleteResponse <- parseRequest ("DELETE http://localhost:8081/" ++ rootItemPath ++ idToDelete) >>= httpBS
  assertStatusCode200 ("Failed to delete item" ++ show idToDelete) deleteResponse

assertWithFoundContent :: Content a => String -> [a] -> Response [Identifiable a] -> IO [Identifiable a]
assertWithFoundContent errorPrefix expectedContents response = do
  assertStatusCode200 errorPrefix response
  assertEqual (errorPrefix ++ "\n\tExpected notes with contents:\n\t" ++ show expectedContents) expectedContents  (map content responseItems)
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

modifyNote :: Content a => a -> Identifiable a -> Identifiable a
modifyNote newContent previousNote = Identifiable (storageId previousNote) newContent


