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
import Data.Monoid (mappend,mconcat,(<>))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Functor ((<$>))
import System.FilePath ((</>))
import System.FilePath.Posix (takeBaseName)

import Text.Parsec
import Control.Applicative hiding ((<|>),many)

---------------------------------------------------------------------------------------------------------

myConfig :: Configuration
myConfig = defaultConfiguration
        { deployCommand = "rsync -rpogtzc --delete -e ssh _site/ collin@rekahsoft.ca:~/public_html/blog/"
        , previewPort = 3000
        }

feedConfig :: FeedConfiguration
feedConfig = feedConfiguration Nothing

main :: IO ()
main = hakyllWith myConfig $ do
-- All Versions ------------------------------------------------------------------------------------------
    match "action/**" $ do
      route   idRoute
      compile copyFileCompiler

    match (fromRegex "^sass/default.s[ac]ss$") $ do
      route   $ gsubRoute "sass/" (const "") `composeRoutes` setExtension "css"
      compile $ getResourceBody >>= withItemBody (fmap compressCss . unixFilter "sass" [])

    match "css/**" $ do
      route   idRoute
      compile compressCssCompiler
    
    match "lib/Skeleton/stylesheets/*.css" $ do
      route   $ gsubRoute "Skeleton/stylesheets" (const "css")
      compile compressCssCompiler

    forM_ [("images/**", idRoute),
           ("fonts/**", idRoute),
           ("lib/Skeleton/images/*",
               gsubRoute "Skeleton" $ const "")] $ \(p, r) ->
      match p $ do
        route   r
        compile copyFileCompiler

    match "templates/**.haml" $ compile $ getResourceBody >>= saveSnapshot "original"
      >>= withItemBody (fmap readTemplate . unixFilter "haml" [])
---------------------------------------------------------------------------------------------------------
-- Default Version --------------------------------------------------------------------------------------
    tags <- buildTags ("posts/**" .&&. hasNoVersion) (fromCapture "tags/*.html")
    tagsRules tags $ genTagRules tags

    -- paginatedPosts <- buildPaginateWith 2 (\n -> fromFilePath $ "blog/page" ++ show n ++ ".html") ("posts/**" .&&. hasNoVersion)
    -- paginatedPosts <- buildPaginate ("posts/**" .&&. hasNoVersion)
    -- paginateRules paginatedPosts (genPaginateRules tags paginatedPosts)

    match "pages/*" $ do
      route   $ setExtension "html"
      compile $ do
        -- Get the current page name
        pageName <- takeBaseName . toFilePath <$> getUnderlying

        posts <- recentFirst =<< loadAllSnapshots ("posts/**" .&&. hasNoVersion) "content"
        
        let recentPosts = take 5 posts
            pageTemplate = "templates/pages/" ++ pageName ++ ".haml"
            masterCtx = listField "recentPosts" (taggedPostCtx tags) (return recentPosts) <>
                        listField "posts" (taggedPostCtx tags) (return posts)             <>
--                        paginateContext paginatedPosts                                    <>
                        tagCloudField "tagCloud" 65 135 tags                              <>
                        defaultContext

        sectionCtx <- getResourceBody >>= genSectionContext
        pg <- loadSnapshot (fromFilePath pageTemplate) "original"
          >>= withItemBody (unixFilter "haml" [])
          >>= applyAsTemplate (sectionCtx <> masterCtx)
          >>= loadAndApplyTemplate "templates/page.haml" defaultContext
        makeItem . itemBody $ pg
        
        -- pandocCompiler
        --   >>= loadAndApplyTemplate (fromFilePath pageTemplate) masterCtx
        --   >>= loadAndApplyTemplate "templates/page.haml" defaultContext
--          >>= relativizeUrls

    -- TODO: add "next" and "previous" while processing templates/partials/post.haml
    match "posts/**" $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/partials/post.haml" (taggedPostCtx tags)
        >>= loadAndApplyTemplate "templates/page.haml" defaultContext
--        >>= relativizeUrls

    create ["atom.xml"] $ do
      route   idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        blogPosts <- loadAllSnapshots ("posts/**" .&&. hasNoVersion) "content"
                       >>= fmap (take 10) . recentFirst
        renderAtom feedConfig feedCtx blogPosts

    forM_ [("js/**", idRoute),
           ("lib/JQuery/*", gsubRoute "JQuery" $ const $ "js"),
           ("lib/jquery-address/src/jquery.address.js",
              customRoute $ const "lib/js/jquery.address.js")] $ \(p, r) ->
      match p $ do
          route   r
          compile $ getResourceString >>= withItemBody (unixFilter "jsmin" [])

    match "index.html" $ do
      route   idRoute
      compile $ do
        -- Generate nav-bar from pages/* ordered by metadata 'weight'
        pages <- sortByM pageWeight =<< loadAll ("pages/*" .&&. hasNoVersion)
            
        let indexCtx = listField "pages" pagesCtx (return pages) <> defaultContext
        
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.haml" indexCtx
          >>= relativizeUrls
---------------------------------------------------------------------------------------------------------
-- NOJS Version -----------------------------------------------------------------------------------------
    -- -- tagsNoJs <- buildTags ("posts/**" .&&. hasVersion "nojs") (fromCapture "nojs/tags/*.html")
    -- -- tagsRules tagsNoJs $ genTagRules tagsNoJs
    
    -- create ["nojs/atom.xml"] $ do
    --   route   idRoute
    --   compile $ do
    --     let feedCtx = postCtx <> bodyField "description"
    --     blogPosts <- loadAllSnapshots ("posts/**" .&&. hasVersion "nojs") "content"
    --                    >>= fmap (take 10) . recentFirst
    --     renderAtom feedConfig feedCtx blogPosts

    -- create ["nojs/archive.html"] $ do
    --   route     idRoute
    --   compile $ do
    --     -- Load all blog posts for archive
    --     posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasVersion "nojs") "content"
        
    --     -- Generate nav-bar from pages/*
    --     pages <- sortByM pageWeight =<< loadAll ("pages/*" .&&. hasVersion "nav-gen")
        
    --     let archiveCtx =
    --           listField "posts" postCtx (return posts) <>
    --           constField "title" "Archives"            <>
    --           defaultContext
    --         indexCtx =
    --           listField "pagesFirst" pagesCtx (return pages) <>
    --           listField "pagesLast" pagesCtx (return [])     <>
    --           defaultContext
        
    --     makeItem ""
    --       >>= loadAndApplyTemplate "templates/archive.haml" archiveCtx
    --       >>= loadAndApplyTemplate "templates/default-nojs.haml" indexCtx
    --       >>= relativizeUrls
    
    -- match "posts/**" $ version "nojs" $ do
    --   route   $ customRoute (\r -> "nojs" </> toFilePath r) `composeRoutes` setExtension "html"
    --   compile $ do
    --     -- Generate nav-bar from pages/*
    --     pages <- sortByM pageWeight =<< loadAll ("pages/*" .&&. hasVersion "nav-gen")

    --     -- Get the current Identifier
    --     curId <- getUnderlying
        
    --     let (pagesFirst, pagesLast') = flip span pages $ \x ->
    --           toFilePath curId /= (toFilePath . itemIdentifier $ x)
    --         pagesLast = if not . null $ pagesLast' then tail pagesLast' else []
    --         postNojsCtx =
    --           listField "pagesFirst" pagesCtx (return pagesFirst) <>
    --           listField "pagesLast" pagesCtx (return pagesLast)   <>
    --           defaultContext
        
    --     pandocCompiler
    --       >>= saveSnapshot "content"
    --       >>= loadAndApplyTemplate "templates/partials/post-nojs.haml" postCtx
    --       >>= loadAndApplyTemplate "templates/default-nojs.haml" postNojsCtx
    --       >>= relativizeUrls

    -- -- This route is used for the initial pass of the pages (nav-gen) and the final nojs page output
    -- let pagesNoJsRoute = customRoute (\r -> if toFilePath r == "pages/home.markdown"
    --                                then "pages/index.markdown"
    --                                else toFilePath r) `composeRoutes`
    --             gsubRoute "pages" (const "nojs")      `composeRoutes`
    --             setExtension "html"
    
    -- match "pages/*" $ version "nav-gen" $ do
    --   route   $ pagesNoJsRoute
    --   compile $ pandocCompiler
    --     >>= loadAndApplyTemplate "templates/page.haml" defaultContext
    
    -- match "pages/*" $ version "nojs" $ do
    --   route   $ pagesNoJsRoute
    --   compile $ do
    --     -- Show a slideshow of blog posts using js..limit to the 3 most recent posts
    --     recentPosts <- loadAllSnapshots ("posts/**" .&&. hasVersion "nojs") "content"
    --                     >>= fmap (take 3) . recentFirst

    --     -- Generate nav-bar from pages/*
    --     pages <- sortByM pageWeight =<< loadAll ("pages/*" .&&. hasVersion "nav-gen")
        
    --     -- Get the current Identifier
    --     curId <- getUnderlying
        
    --     let (pagesFirst, pagesLast') = flip span pages $ \x ->
    --           toFilePath curId /= (toFilePath . itemIdentifier $ x)
    --         pageMid = head pagesLast'
    --         pagesLast = if not . null $ pagesLast' then tail pagesLast' else []
    --         pagesNojsCtx =
    --           listField "recentPosts" postCtx (return recentPosts)       <>
    --           listField "pagesFirst" pagesCtx (return pagesFirst)        <>
    --           field "pageMid" (const $ return . itemBody $ pageMid)      <>
    --           listField "pagesLast" pagesCtx (return pagesLast)          <>
    --           defaultContext

    --     loadVersion "nav-gen" curId
    --       >>= loadAndApplyTemplate "templates/default-nojs.haml" pagesNojsCtx
    --       >>= relativizeUrls
---------------------------------------------------------------------------------------------------------
-- Functions & Constants --------------------------------------------------------------------------------
feedConfiguration :: Maybe String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle = title'
    , feedDescription = "My encounters with math, programming, science and the world!"
    , feedAuthorName = "Collin J. Doering"
    , feedAuthorEmail = "support@rekahsoft.ca"
    , feedRoot = "http://blog.rekahsoft.ca"
    } where title' = maybe defaultTitle ((defaultTitle ++ "; Specifically on the topic of ") ++) title
            defaultTitle = "Technical Musings of a Minimalist"

genTagRules :: Tags -> String -> Pattern -> Rules ()
genTagRules tags tag pattern = do
  route   $ gsubRoute " " (const "-")
  compile $ do
    posts <- recentFirst =<< loadAllSnapshots pattern "content"
    let tagPageCtx = listField "posts" (taggedPostCtx tags) (return posts) <>
                     tagCloudField "tagCloud" 65 135 tags <>
                     constField "tag" tag

    makeItem ""
      >>= loadAndApplyTemplate "templates/tag-page.haml" tagPageCtx
      >>= loadAndApplyTemplate "templates/page.haml" defaultContext
  
  version "rss" $ do
    route   $ gsubRoute " " (const "-") `composeRoutes` setExtension "xml"
    compile $ loadAllSnapshots pattern "content"
      >>= fmap (take 10) . recentFirst
      >>= renderAtom (feedConfiguration $ Just tag) (bodyField "description" <> defaultContext)

-- genPaginateRules :: Tags -> Paginate -> PageNumber -> Pattern -> Rules ()
-- genPaginateRules tags paginate n pattern = do
--   route   $ idRoute
--   compile $ pandocCompiler
--     >>= loadAndApplyTemplate "templates/partials/post.haml" (taggedPostCtx tags <> paginateContext paginate)
--     >>= loadAndApplyTemplate "templates/page.haml" defaultContext
--     >>= relativizeUrls

loadVersion :: String -> Identifier -> Compiler (Item String)
loadVersion v i = load (setVersion (listAsMaybe v) i) >>= makeItem . itemBody
  where listAsMaybe [] = Nothing
        listAsMaybe xs = Just xs

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"   <>
          teaserField "teaser" "content" <>
          field "virtualpath" (fmap (maybe "" toUrl) . getRoute . itemIdentifier) <>
          defaultContext

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = tagsField "tags" tags <> postCtx
  
pagesCtx :: Context String
pagesCtx = field "virtualpath" (fmap (drop 6 . maybe "" toUrl) . getRoute . itemIdentifier) <>
           defaultContext

pageWeight :: (Functor f, MonadMetadata f) => Item a -> f Int
pageWeight i = fmap (maybe 0 read) $ getMetadataField (itemIdentifier i) "weight"

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                 mapM (\x -> liftM (x,) (f x)) xs
---------------------------------------------------------------------------------------------------------
-- WORKING PROGRESS
---------------------------------------------------------------------------------------------------------
--genSectionContext :: Item String -> Compiler (Context String)
genSectionContext = fmap mconcat . sequence . map makeField . unSections . readSections . itemBody
  where makeField (k, b) = constField k . itemBody <$> (makeItem b >>= return . writePandoc . readPandoc)

--readSections :: String -> Compiler (Context String)
readSections :: String -> [Section String String]
readSections s = case parse sections "" s of
  Left err -> error $ "Cannot parse sections: " ++ show err
  Right t -> t

data Section k a = NonSection a
                 | Section k a
                 | GlobalSection a
                 deriving (Eq, Show)

instance Functor (Section k) where
  fmap f (NonSection b) = NonSection $ f b
  fmap f (Section k b) = Section k (f b)
  fmap f (GlobalSection b) = GlobalSection $ f b

unSections :: [Section String String] -> [(String, String)]
unSections = snd . foldr unSection (0, []) . filter (not . isGlobalSection) . applyGlobalSections
  where unSection (NonSection b) (n, ys) = (n + 1, ("body" ++ show n, b) : ys)
        unSection (Section k b) (n, ys) = (n, (k, b) : ys)
        unSection (GlobalSection _) _ = error "Internal error! This should never happen!"

isGlobalSection :: Section k a -> Bool
isGlobalSection (GlobalSection _) = True
isGlobalSection _ = False

applyGlobalSections :: [Section k String] -> [Section k String]
applyGlobalSections xs = flip map xs $ fmap $ trim . (++globalSectionBody)
  where globalSectionBody = let gs = foldr unGlobalSection [] xs in
          if null gs then gs else "\n" ++ gs
        unGlobalSection (GlobalSection b) ys
          | null ys = b
          | otherwise = b ++ "\n" ++ ys
        unGlobalSection _ ys = ys

-- **TODO** parser needs to be completed below
sections :: Parsec String a [Section String String]
sections = many (section <|> globalSection <|> nonSection)

section :: Parsec String a (Section String String)
section = do
  key <- sectionStart
  body <- sectionBody
  sectionEnd
  return $ Section key body

sectionStart :: Parsec String a String
sectionStart = between (try $ string "$section(") (string ")$") sectionId

sectionId :: Parsec String a String
sectionId = between (char '"') (char '"') (many1 (noneOf "\""))

sectionEnd :: Parsec String a String
sectionEnd = string "$endsection$"

sectionBody :: Parsec String a String
sectionBody = try escapedDollar <|> many1 (noneOf "$")

escapedDollar :: Parsec String a String
escapedDollar = try (between (char '$') (char '$')
                     (try ((sectionStart >>= (\s -> return $ "$section(\"" ++ s ++ "\")$")) <|>
                            sectionEnd <|> string "$section$"))) <|>
                try sectionEnd                                             <|>
                (string "$$" *> return "$")

globalSection :: Parsec String a (Section k String)
globalSection = GlobalSection <$> between (string "$section$") sectionEnd sectionBody

nonSection :: Parsec String a (Section k String)
nonSection = many1 (many1 (noneOf "$") <|> try escapedDollar) >>= return . NonSection . concat

