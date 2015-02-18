-- (C) Copyright Collin J. Doering 2015
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- File: Main.hs
-- Author: Collin J. Doering <collin.doering@rekahsoft.ca>
-- Date: Feb  4, 2015

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket)
import           Control.Monad
import qualified Data.Text as T
import           System.Process
import           System.Directory (getDirectoryContents)
import           Test.Hspec.WebDriver
import qualified Test.WebDriver.Commands.Wait as WDw
import qualified Test.WebDriver.Session as WDs

runSiteServerWith :: IO a -> IO ()
runSiteServerWith action = void $ bracket makeSiteProc terminateProcess (const action)
 where makeSiteProc = do
         let cp = shell "./site server"
             cp' = cp { std_out = CreatePipe
                      , std_err = CreatePipe }
         (_, _, _, ph) <- createProcess $ cp'

         -- Pause for a moment to let http server start
         threadDelay 250000

         return ph

siteUrl :: String
siteUrl = "http://localhost:3000"

waitTime :: Double
waitTime = 10

secsUntil :: WDs.WDSessionState m => Double -> m a -> m ()
secsUntil t a = void $ WDw.waitUntil t a

waitUntil :: WDs.WDSessionState m => m a -> m ()
waitUntil = secsUntil waitTime

secsWhile :: WDs.WDSessionState m => Double -> m a -> m ()
secsWhile = WDw.waitWhile

waitWhile :: WDs.WDSessionState m => m a -> m ()
waitWhile = secsWhile waitTime

ensurePageLoaded :: WD ()
ensurePageLoaded = do
  waitUntil $ findElem $ ById "page-content"
  waitWhile $ findElem $ ByCSS "#page-content.loading"
  waitWhile $ findElem $ ByCSS "#status.error"

main :: IO ()
main = runSiteServerWith $ hspec $ do
  describe "RekahSoft Blog Tests" $ do

    -- session "for application cache" $ using [Firefox, Chrome] $ do
    --   it "has a fresh cache manifest" $ do
    --     pending

    parallel $ session "general site navigation tests" $ using [Firefox, Chrome] $ do
      describe "navigation menuitems" $ do
         context "when clicked" $ do
           it "load the expected page and sets the menuitems parent as active" $ runWD $ do
             openPage siteUrl
             navItems <- findElems $ ByCSS "#nav a.menuitem"

             flip mapM_ navItems $ \item -> do
               itemHrefMaybe <- attr item "href"
               itemHref <- case itemHrefMaybe of
                            Nothing   -> fail "No href given on menuitem"
                            Just href -> return $ T.drop (length siteUrl) href

               click item
               ensurePageLoaded

               activeItem <- findElem $ ByCSS $ "#nav li.active > a.menuitem[href=\"" `T.append` itemHref `T.append` "\"]"
               activeItem `shouldBeTag` "a"

      describe "blog Page" $ do
        it "is paginated" $ runWD $ do
          openPage $ siteUrl ++ "/#/blog.html"
          ensurePageLoaded

          pagination     <- findElem $ ById "pagination"
          noFirstPage    <- findElemFrom pagination $ ByCSS "span.on-first-page"
          noPreviousPage <- findElemFrom pagination $ ByCSS "span.no-previous-page"

          pagination     `shouldBeTag` "div"
          noFirstPage    `shouldBeTag` "span"
          noPreviousPage `shouldBeTag` "span"

      describe "invalid virtual URLS" $ do
        context "when requested" $ do
          it "show a error message dialog" $ runWD $ do
            openPage $ siteUrl ++ "/#/thispagedoesnotexist.html"
            waitUntil $ (findElem (ById "status") >>= isDisplayed)

      describe "loads all pages (html files in pages/*)" $ do
        pages <- runIO $ liftM (filter (flip notElem [".", ".."])) $ getDirectoryContents "_site/pages"

        it "opens the app index page" $ runWD $ do
          openPage siteUrl
          ensurePageLoaded

        flip mapM_ pages $ \page -> do
          it ("opens the " ++ page ++ " page") $ runWD $ do
            openPage $ siteUrl ++ "/#/" ++ page
            ensurePageLoaded

      describe "Loads all posts (html files in posts/*)" $ do
        posts <- runIO $ liftM (filter (flip notElem [".", ".."])) $ getDirectoryContents "_site/posts"

        flip mapM_ posts $ \post -> do
          it ("opens post with filename " ++ post) $ runWD $ do
            openPage $ siteUrl ++ "/#/posts/" ++ post
            ensurePageLoaded

    parallel $ session "Initialization tests" $ using [Firefox, Chrome] $ do
      it "shows a error message in page-content when an invalid url is accessed" $ runWD $ do
        openPage $ siteUrl ++ "/#/thisPageDoesNotExist.html"

        waitUntil $ findElem $ ById "page-content"
        waitUntil $ findElem $ ByCSS "#page-content.init.loading-error"
