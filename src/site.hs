---------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts #-}
---------------------------------------------------------------------------------------------------------
-- (C) Copyright Collin Doering 2013
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
-- Description: The static site generator for my personal technical blog
---------------------------------------------------------------------------------------------------------

import Hakyll
import Control.Monad
import Data.Monoid (mconcat,(<>))
import Data.List (sortBy)
import Data.Map (toList, size)
import qualified Data.Set as S
import Data.Ord (comparing)
import System.Random
import System.FilePath (takeBaseName)
import System.Process
import System.Exit
import System.IO (hGetContents)

import Text.Parsec
import Text.Pandoc.Options
import Control.Applicative hiding ((<|>),many)

---------------------------------------------------------------------------------------------------------

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions
                  { readerExtensions = S.fromList
                                       [ Ext_footnotes
                                       , Ext_inline_notes
                                       , Ext_pandoc_title_block
                                       , Ext_yaml_metadata_block
                                       , Ext_table_captions
                                       , Ext_implicit_figures
                                       , Ext_simple_tables
                                       , Ext_multiline_tables
                                       , Ext_grid_tables
                                       , Ext_pipe_tables
                                       , Ext_citations
                                       , Ext_raw_tex
                                       , Ext_raw_html
                                       , Ext_tex_math_dollars
                                       , Ext_latex_macros
                                       , Ext_fenced_code_blocks
                                       , Ext_fenced_code_attributes
                                       , Ext_backtick_code_blocks
                                       , Ext_inline_code_attributes
                                       , Ext_markdown_in_html_blocks
                                       , Ext_escaped_line_breaks
                                       , Ext_fancy_lists
                                       , Ext_startnum
                                       , Ext_definition_lists
                                       , Ext_example_lists
                                       , Ext_all_symbols_escapable
                                       , Ext_intraword_underscores
                                       , Ext_blank_before_blockquote
                                       , Ext_blank_before_header
                                       , Ext_strikeout
                                       , Ext_superscript
                                       , Ext_subscript
                                       , Ext_auto_identifiers
                                       , Ext_header_attributes
                                       , Ext_implicit_header_references
                                       , Ext_line_blocks ]
                  }

pandocWriterOptions :: WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
                      { writerHtml5 = True
                      , writerHTMLMathMethod = MathJax ""
                      , writerEmailObfuscation = NoObfuscation -- ReferenceObfuscation
                      }

myConfig :: Configuration
myConfig = defaultConfiguration
        { deployCommand = "echo 'Removing empty files...' && " ++
                          "find _site -type f -empty -exec rm -v {} \\; && " ++
                          "echo '\nDeploying website...' && " ++
                          "rsync -rpogtzcv --delete -e ssh _site/ collin@rekahsoft.ca:~/public_html/blog/"
        , previewPort = 3000
        }

main :: IO ()
main = do
  -- Get a random number generator before going into Rules monad
  stdGen <- getStdGen

  hakyllWith myConfig $ do
-- All Versions ------------------------------------------------------------------------------------------
    match ("action/**" .||. "files/**" .||. "images/**" .||. "fonts/**") $ do
      route   idRoute
      compile copyFileCompiler

    tags <- buildTags ("posts/**" .&&. hasNoVersion) (fromCapture "tags/*.html")

    paginatedPosts <- buildPaginateWith
                      (fmap (paginateEvery numPaginatePages) . sortRecentFirst)
                      ("posts/**" .&&. hasNoVersion)
                      (\n -> fromCapture "pages/blog*.html" (show n))

    pageIds  <- getMatches ("pages/**" .&&. complement "pages/blog.markdown")
    fontIds  <- getMatches "fonts/**"
    imageIds <- getMatches "images/**"
    cssIds   <- getMatches "css/**"
    jsIds    <- getMatches "js/**"
    libIds   <- getMatches "lib/**"

    clayIds <- getMatches "clay/**.hs"
    let manifestIds = clayIds ++ fontIds ++ imageIds ++ pageIds ++ cssIds ++ libIds ++ jsIds

    clayDeps     <- makePatternDependency $ fromList clayIds
    manifestDeps <- makePatternDependency $ fromList manifestIds

    rulesExtraDependencies [clayDeps] $ create ["default.css"] $ do
      route     idRoute
      compile $ makeItem =<< (unsafeCompiler $ do
        (_, hout, _, ph) <- createProcess $ shell "cabal build gencss"
        exitCode <- waitForProcess ph
        if exitCode == ExitSuccess
           then readProcess "cabal" ["run", "--verbose=0", "gencss", "compact"] ""
           else case hout of
                 Nothing -> fail "Error running 'cabal build gencss'"
                 Just hout' -> hGetContents hout' >>= fail)

    rulesExtraDependencies [manifestDeps] $ create ["manifest.appcache"] $ do
      route     idRoute
      compile $ do
        manifestCacheRoutesMaybe <- sequence $ liftM getRoute (fontIds ++ pageIds ++ imageIds ++ cssIds ++ libIds ++ jsIds)
        let randomNum = random stdGen :: (Int, StdGen)
            randomStr = show . abs . fst $ randomNum
            manifestStart = [ "CACHE MANIFEST"
                            , "# " ++ randomStr ]
            manifestCacheSingles = [ "/index.html"
                                   , "/default.css" ]
            paginatedPostsCache = take 2 $ map (\(n,_) -> "/pages/blog" ++ (show n) ++ ".html") $ toList $ paginateMap paginatedPosts
            tagsCache = concatMap (\(t,ids) -> take 2 $ ["/tags/" ++ t ++ show n ++ ".html" | n <- [1..length $ paginateEvery numPaginatePages ids]]) $ tagsMap tags
            manifestCacheFromIds = filter (not . null) $ fmap (maybe "" ("/"++)) manifestCacheRoutesMaybe
            manifestCache = manifestCacheFromIds ++ tagsCache ++ paginatedPostsCache
            manifestNetwork = [ "NETWORK:"
                              , "*"
                              , "http://*"
                              , "https://*" ]
        makeItem . unlines $ manifestStart ++ [""] ++
                             manifestCacheSingles ++ manifestCache ++ [""] ++
                             manifestNetwork ++ [""]

    match "css/**" $ do
      route   idRoute
      compile compressCssCompiler

    match "lib/Skeleton/*.css" $ do
      route   $ gsubRoute "Skeleton" (const "css")
      compile compressCssCompiler

    match "templates/**" $ compile $ getResourceBody >>= saveSnapshot "original"
      >> templateCompiler

---------------------------------------------------------------------------------------------------------
-- Default Version --------------------------------------------------------------------------------------
    -- Generate tag pages
    paginateTagsRules tags

    paginateRules paginatedPosts $ \pageNum pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots pattern "content"
        let ctx = taggedPostCtx tags <>
                  paginateContext paginatedPosts pageNum <>
                  virtualPaginateContext paginatedPosts pageNum <>
                  constField "weight" "0" <>
                  listField "posts" (taggedPostCtx tags) (return posts)
        makeItem ""
          >>= loadAndApplyTemplate "templates/pages/blog.html" ctx

    match "pages/*" $ do
      route   $ setExtension "html"
      compile $ do
        -- Get the current page name
        pageName <- takeBaseName . toFilePath <$> getUnderlying

        posts <- recentFirst =<< loadAllSnapshots ("posts/**" .&&. hasNoVersion) "content"

        let recentPosts = take 5 posts
            pageTemplate = "templates/pages/" ++ pageName ++ ".html"
            masterCtx = listField "recentPosts" (taggedPostCtx tags) (return recentPosts) <>
                        listField "posts" (taggedPostCtx tags) (return posts)             <>
                        tagCloudField "tagCloud" 65 135 tags                              <>
                        defaultContext

        sectionCtx <- getResourceBody >>= genSectionContext
        pg <- loadSnapshot (fromFilePath pageTemplate) "original"
          >>= applyAsTemplate (sectionCtx <> masterCtx)

        if pageName == "blog"
          then makeItem ""
          else makeItem . itemBody $ pg

    match "posts/**" $ do
      route   $ setExtension "html"
      compile $ pandocCompilerWith pandocReaderOptions pandocWriterOptions
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/partials/post.html" (taggedPostCtx tags)

    create ["atom.xml"] $ do
      route   idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        blogPosts <- loadAllSnapshots ("posts/**" .&&. hasNoVersion) "content"
                       >>= fmap (take 10) . recentFirst
        renderAtom (feedConfiguration Nothing) feedCtx blogPosts

    forM_ [("js/**", idRoute),
           ("lib/JQuery/*", gsubRoute "JQuery" $ const "js"),
           ("lib/jquery-address/jquery.address.js",
              customRoute $ const "lib/js/jquery.address.js")] $ \(p, r) ->
      match p $ do
          route   r
          compile $ getResourceString >>= withItemBody (unixFilter "jsmin" [])

    create ["index.html"] $ do
      route   idRoute
      compile $ do
        -- Generate nav-bar from pages/* ordered by metadata 'weight'
        pages <- sortByM pageWeight =<< filterM (\i -> pageWeight i >>= return . (> 0)) =<< loadAll ("pages/*" .&&. hasNoVersion)

        let indexCtx = listField "pages" pagesCtx (return pages) <> defaultContext

        makeItem "loading"
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx

---------------------------------------------------------------------------------------------------------
-- Functions & Constants --------------------------------------------------------------------------------
feedConfiguration :: Maybe String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle = title'
    , feedDescription = "My encounters with math, programming, science and the world!"
    , feedAuthorName = "Collin J. Doering"
    , feedAuthorEmail = "collin.doering@rekahsoft.ca"
    , feedRoot = "http://blog.rekahsoft.ca"
    } where title' = maybe defaultTitle ((defaultTitle ++ "; Specifically on the topic of ") ++) title
            defaultTitle = "Technical Musings of a Minimalist"

numPaginatePages :: Int
numPaginatePages = 6

paginateTagsRules :: Tags -> Rules ()
paginateTagsRules tags =
  forM_ (tagsMap tags) $ \(tag, identifiers) -> do
    paginatedTaggedPosts <- buildPaginateWith
                            (fmap (paginateEvery numPaginatePages) . sortRecentFirst)
                            (fromList identifiers)
                            (\n -> fromCapture (fromGlob $ "tags/" ++ tag ++ "*.html") (show n))

    paginateRules paginatedTaggedPosts $ \pageNum pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots pattern "content"
        let ctx = taggedPostCtx tags                                    <>
                  paginateContext paginatedTaggedPosts pageNum          <>
                  constField "tag" tag                                  <>
                  listField "posts" (taggedPostCtx tags) (return posts)
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag-page.html" ctx

    rulesExtraDependencies [tagsDependency tags] $ do
      create [tagsMakeId tags tag] $ do
        route   $ gsubRoute " " (const "-")
        compile $ makeItem ("" :: String)

        version "rss" $ do
          route   $ gsubRoute " " (const "-") `composeRoutes` setExtension "xml"
          compile $ loadAllSnapshots (fromList identifiers) "content"
            >>= fmap (take 10) . recentFirst
            >>= renderAtom (feedConfiguration $ Just tag) (bodyField "description" <> defaultContext)

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"   <>
          teaserField "teaser" "content" <>
          field "virtualpath" (fmap (drop 6 . maybe "" toUrl) . getRoute . itemIdentifier) <>
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

-- This is copied verbatim from Web/Paginate.hs as it isn't exported
-- Get the identifier for a certain page by passing in the page number.
paginatePage :: Paginate -> PageNumber -> Maybe Identifier
paginatePage pag pageNumber
    | pageNumber < 1                      = Nothing
    | pageNumber > (paginateNumPages pag) = Nothing
    | otherwise                           = Just $ paginateMakeId pag pageNumber

-- This is copied verbatim from Web/Paginate.hs as it isn't exported
paginateNumPages :: Paginate -> Int
paginateNumPages = size . paginateMap

virtualPaginateContext :: Paginate -> PageNumber -> Context a
virtualPaginateContext pag currentPage = mconcat
    [ field "firstPageUrlVirtualPath"    $ \_ -> otherPage 1                 >>= url
    , field "previousPageUrlVirtualPath" $ \_ -> otherPage (currentPage - 1) >>= url
    , field "nextPageUrlVirtualPath"     $ \_ -> otherPage (currentPage + 1) >>= url
    , field "lastPageUrlVirtualPath"     $ \_ -> otherPage lastPage          >>= url
    ]
  where
    lastPage = paginateNumPages pag

    otherPage n
        | n == currentPage = fail $ "This is the current page: " ++ show n
        | otherwise        = case paginatePage pag n of
            Nothing -> fail $ "No such page: " ++ show n
            Just i  -> return (n, i)

    url :: (Int, Identifier) -> Compiler String
    url (n, i) = getRoute i >>= \mbR -> case mbR of
        Just r  -> return $ drop 6 . toUrl $ r
        Nothing -> fail $ "No URL for page: " ++ show n

---------------------------------------------------------------------------------------------------------
-- Page section parser
---------------------------------------------------------------------------------------------------------
genSectionContext :: Item String -> Compiler (Context a)
genSectionContext = fmap mconcat . sequence . map makeField . unSections . readSections . itemBody
  where makeField (k, b) = constField k . itemBody <$> (makeItem b >>= readPandocWith pandocReaderOptions >>= return . writePandocWith pandocWriterOptions)

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
  where unSection :: Section String String -> (Integer, [(String, String)]) -> (Integer, [(String, String)])
        unSection (NonSection b) (n, ys) = (n + 1, ("body" ++ show n, b) : ys)
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

-- **TODO** parser has error with escapes within pages; that is $$someExample$$ and $$section("example")$$
sections :: Parsec String a [Section String String]
sections = many (section <|> globalSection <|> nonSection)

section :: Parsec String a (Section String String)
section = do
  key <- sectionStart
  body <- sectionBody
  void $ sectionEnd
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
