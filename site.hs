---------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
---------------------------------------------------------------------------------------------------------
-- (C) Copyright Collin Doering @!@YEAR@!@
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

-- File: site.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Oct 11, 2013
-- Description: The website for RekahSoft
---------------------------------------------------------------------------------------------------------

import Hakyll
import Control.Monad
import Data.Monoid (mappend)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Functor ((<$>))
import System.FilePath ((</>))

---------------------------------------------------------------------------------------------------------

myConfig :: Configuration
myConfig = defaultConfiguration
        { deployCommand = "rsync -rpogtzc --delete -e ssh _site/ collin@omicron:~/public_html/"
        , previewPort = 3000
        }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle = "RekahSoft updates and news"
    , feedDescription = "Updates and news in regards to RekahSoft"
    , feedAuthorName = "Collin J. Doering"
    , feedAuthorEmail = "support@rekahsoft.ca"
    , feedRoot = "http://rekahsoft.ca"
    }

main :: IO ()
main = hakyllWith myConfig $ do
-- All Versions ------------------------------------------------------------------------------------------
    match "action/**" $ do
      route   idRoute
      compile copyFileCompiler

    forM_ [("css/**", idRoute),
           ("bootstrap/dist/css/bootstrap.css", customRoute $ const "lib/css/bootstrap.css")] $ \(p, r) ->
      match p $ do
        route   r
        compile compressCssCompiler

    forM_ [("images/**", idRoute),
           ("bootstrap/assets/ico/favicon.png",
               customRoute $ const "lib/ico/favicon.png")] $ \(p, r) ->
      match p $ do
        route   r
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler
---------------------------------------------------------------------------------------------------------
-- Default Version --------------------------------------------------------------------------------------
    match "pages/*" $ do
      route   $ setExtension "html"
      compile $ do
        pandocCompiler
          >>= loadAndApplyTemplate "templates/page.html" defaultContext
          >>= relativizeUrls

    match "news/**" $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= saveSnapshot "content"
--        >>= loadAndApplyTemplate "templates/news.html" newsCtx
        >>= relativizeUrls

    create ["atom.xml"] $ do
      route   idRoute
      compile $ do
        let feedCtx = newsCtx `mappend` bodyField "description"
        newsPosts <- loadAllSnapshots ("news/**" .&&. hasNoVersion) "content"
                       >>= fmap (take 10) . recentFirst
        renderAtom feedConfig feedCtx newsPosts
    
    create ["news-archive.html"] $ do
      route   idRoute
      compile $ do
        news <- recentFirst =<< loadAllSnapshots ("news/*" .&&. hasNoVersion) "content"
        let archiveCtx =
              listField "news" newsCtx (return news) `mappend`
              constField "title" "News Archives"     `mappend`
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/page.html" defaultContext
          >>= relativizeUrls

    create ["recent-news.html"] $ do
      route   $ constRoute "recent-news.html"
      compile $ do
        -- Show a slideshow of news using js..limit to the 5 most recent posts
        recentNews' <- loadAllSnapshots ("news/**" .&&. hasNoVersion) "content"
                         >>= fmap (take 5) . recentFirst
            
        let mostRecentNews = head recentNews'
            recentNews = tail recentNews'
            recentNewsCtx =
              field "mostRecentNewsTeaser" (const $ do
                body <- itemBody <$> loadSnapshot (itemIdentifier mostRecentNews) "content"
                case needlePrefix "<!--more-->" body of
                  Nothing -> fail $
                               "rekahsoft-ca (site.hs): No teaser defined for " ++
                               show (itemIdentifier mostRecentNews)
                  Just s -> return s)                                               `mappend`
              field "mostRecentNewsBody"
                (const $ return . itemBody $ mostRecentNews)                        `mappend`
              field "mostRecentNewsTitle"
                (const $ getMetadataField' (itemIdentifier mostRecentNews) "title") `mappend`
              listField "recentNews" newsTeaserCtx (return recentNews)              `mappend`
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/recent-news.html" recentNewsCtx
          >>= relativizeUrls

    forM_ [("js/**", idRoute),
           ("bootstrap/dist/js/bootstrap.js", customRoute $ const "lib/js/bootstrap.js"),
           ("bootstrap/assets/js/jquery.js", customRoute $ const "lib/js/jquery.js"),
           ("bootstrap/js/*.js", gsubRoute "bootstrap" (const "lib")),
           ("jquery-address/src/jquery.address.js",
            customRoute $ const "lib/js/jquery.address.js")] $ \(p, r) ->
      match p $ do
          route   r
          compile $ getResourceString >>= withItemBody (unixFilter "jsmin" [])

    match "index.html" $ do
      route   idRoute
      compile $ do
        -- Generate nav-bar from pages/*
        pages <- sortByM pageWeight =<< loadAll ("pages/*" .&&. hasNoVersion)
            
        let indexCtx = listField "pages" pagesCtx (return pages) `mappend` defaultContext
            
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls
---------------------------------------------------------------------------------------------------------
-- NOJS Version -----------------------------------------------------------------------------------------
    create ["nojs/atom.xml"] $ do
      route   idRoute
      compile $ do
        let feedCtx = newsCtx `mappend` bodyField "description"
        newsPosts <- loadAllSnapshots ("news/**" .&&. hasVersion "nojs") "content"
                       >>= fmap (take 10) . recentFirst
        renderAtom feedConfig feedCtx newsPosts

    create ["nojs/news-archive.html"] $ do
      route     idRoute
      compile $ do
        -- Load all news for archive
        news <- recentFirst =<< loadAllSnapshots ("news/*" .&&. hasVersion "nojs") "content"
        
        -- Generate nav-bar from pages/*
        pages <- sortByM pageWeight =<< loadAll ("pages/*" .&&. hasVersion "nav-gen")
        
        let archiveCtx =
              listField "news" newsCtx (return news) `mappend`
              constField "title" "News Archives"     `mappend`
              defaultContext
            indexCtx =
              listField "pagesFirst" pagesCtx (return pages)     `mappend`
              listField "pagesLast" pagesCtx (return [])         `mappend`
              defaultContext
        
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default-nojs.html" indexCtx
          >>= relativizeUrls
    
    match "news/**" $ version "nojs" $ do
      route   $ customRoute (\r -> "nojs" </> toFilePath r) `composeRoutes` setExtension "html"
      compile $ do
        -- Generate nav-bar from pages/*
        pages <- sortByM pageWeight =<< loadAll ("pages/*" .&&. hasVersion "nav-gen")

        -- Get the current Identifier
        curId <- getUnderlying
        
        let (pagesFirst, pagesLast') = flip span pages $ \x ->
              toFilePath curId /= (toFilePath . itemIdentifier $ x)
            pagesLast = if not . null $ pagesLast' then tail pagesLast' else []
            newsNojsCtx =
              listField "pagesFirst" pagesCtx (return pagesFirst) `mappend`
              listField "pagesLast" pagesCtx (return pagesLast)   `mappend`
              defaultContext
        
        pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/news-nojs.html" newsCtx
          >>= loadAndApplyTemplate "templates/default-nojs.html" newsNojsCtx
          >>= relativizeUrls

    -- This route is used for the initial pass of the pages (nav-gen) and the final nojs page output
    let pagesNoJsRoute = customRoute (\r -> if toFilePath r == "pages/home.markdown"
                                   then "pages/index.markdown"
                                   else toFilePath r) `composeRoutes`
                gsubRoute "pages" (const "nojs")      `composeRoutes`
                setExtension "html"
    
    match "pages/*" $ version "nav-gen" $ do
      route   $ pagesNoJsRoute
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html" defaultContext
    
    match "pages/*" $ version "nojs" $ do
      route   $ pagesNoJsRoute
      compile $ do
        -- Show a slideshow of news using js..limit to the 3 most recent posts
        recentNews <- loadAllSnapshots ("news/**" .&&. hasVersion "nojs") "content"
                        >>= fmap (take 3) . recentFirst

        -- Generate nav-bar from pages/*
        pages <- sortByM pageWeight =<< loadAll ("pages/*" .&&. hasVersion "nav-gen")
        
        -- Get the current Identifier
        curId <- getUnderlying
        
        let (pagesFirst, pagesLast') = flip span pages $ \x ->
              toFilePath curId /= (toFilePath . itemIdentifier $ x)
            pageMid = head pagesLast'
            pagesLast = if not . null $ pagesLast' then tail pagesLast' else []
            pagesNojsCtx =
              listField "recentNews" newsTeaserCtx (return recentNews)    `mappend`
              listField "pagesFirst" pagesCtx (return pagesFirst)   `mappend`
              field "pageMid" (const $ return . itemBody $ pageMid) `mappend`
              listField "pagesLast" pagesCtx (return pagesLast)     `mappend`
              defaultContext

        loadVersion "nav-gen" curId
          >>= loadAndApplyTemplate "templates/default-nojs.html" pagesNojsCtx
          >>= relativizeUrls
---------------------------------------------------------------------------------------------------------
-- Functions & Constants --------------------------------------------------------------------------------
loadVersion :: String -> Identifier -> Compiler (Item String)
loadVersion v i = load (setVersion (listAsMaybe v) i) >>= makeItem . itemBody
  where listAsMaybe [] = Nothing
        listAsMaybe xs = Just xs

newsCtx :: Context String
newsCtx = dateField "date" "%B %e, %Y" `mappend`
          defaultContext

newsTeaserCtx :: Context String
newsTeaserCtx = teaserField "teaser" "content" `mappend`
                newsCtx
  
pagesCtx :: Context String
pagesCtx = field "virtualpath" (fmap (drop 6 . maybe "" toUrl) . getRoute . itemIdentifier) `mappend`
           defaultContext

pageWeight :: (Functor f, MonadMetadata f) => Item a -> f Int
pageWeight i = fmap (maybe 0 read) $ getMetadataField (itemIdentifier i) "weight"

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                 mapM (\x -> liftM (x,) (f x)) xs
---------------------------------------------------------------------------------------------------------
