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
import Data.List (sortBy)
import Data.Ord (comparing)
import System.FilePath (takeBaseName)
import System.Process

import Text.Parsec
import Text.Pandoc.Options
import Control.Applicative()

---------------------------------------------------------------------------------------------------------

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def
                  { readerExtensions = extensionsFromList
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
                      { writerHTMLMathMethod = MathJax ""
                      , writerEmailObfuscation = NoObfuscation -- ReferenceObfuscation
                      }

myConfig :: Configuration
myConfig = defaultConfiguration
        { deployCommand = "echo 'TODO (what to do with this cmd) Deploying website...' && " ++
                          "aws s3 sync _site/ s3://$S3_BUCKET &&"  ++
                          "echo 'Done!'"
        , previewPort = 3000
        }

siteRules :: Rules ()
siteRules = do
  match ("js/**"
         .||. "files/**"
         .||. "images/**"
         .||. "fonts/**"
         .||. "robots.txt") $ do
    route   idRoute
    compile copyFileCompiler

  forM_ [("lib/MathJax/fonts/HTML-CSS/**", gsubRoute "lib/MathJax/" $ const ""),
         ("lib/MathJax/**" .&&. complement "lib/MathJax/fonts", gsubRoute "lib/" $ const ""),
         ("lib/JQuery/*", gsubRoute "JQuery" $ const "js")] $ \(p, r) ->
    match p $ do
        route   r
        compile $ copyFileCompiler

  match "css/**" $ do
    route   idRoute
    compile compressCssCompiler

  match "lib/Skeleton/*.css" $ do
    route   $ gsubRoute "Skeleton" (const "css")
    compile compressCssCompiler

  match "templates/**" $ compile $ getResourceBody >>= saveSnapshot "original"
    >> templateCompiler

  -- Generate tags
  tags <- buildTags ("posts/**" .&&. hasNoVersion) (fromCapture "tags/*1.html")

  -- Generate paginate
  paginatedPosts <- buildPaginateWith
                    (fmap (paginateEvery numPaginatePages) . sortRecentFirst)
                    ("posts/**" .&&. hasNoVersion)
                    (\n -> fromCapture "blog*.html" (show n))

  clayDeps <- makePatternDependency $ fromGlob "clay/*.hs"

  rulesExtraDependencies [clayDeps] $ create ["default.css"] $ do
    route     idRoute
    compile $ makeItem =<< (unsafeCompiler $ readProcess "gencss" ["compact"] "")

  -- Generate tag pages
  forM_ (tagsMap tags) $ \(tag, identifiers) -> do
    paginatedTaggedPosts <- buildPaginateWith
                            (fmap (paginateEvery numPaginatePages) . sortRecentFirst)
                            (fromList identifiers)
                            (\n -> fromCapture (fromGlob $ "tags/" ++ tag ++ "*.html") (show n))

    paginateRules paginatedTaggedPosts $ \pageNum pattern -> do
      route   $ gsubRoute " " (const "-") `composeRoutes` setExtension "html"
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots pattern "content"

        navCtx <- genNavContext "pages/blog.markdown"

        let ctx = taggedPostCtx tags                                    <>
                  paginateContext paginatedTaggedPosts pageNum          <>
                  constField "tag" tag                                  <>
                  listField "posts" (taggedPostCtx tags) (return posts)
            indexCtx = navCtx

        makeItem ""
          >>= loadAndApplyTemplate "templates/tag-page.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx

    rulesExtraDependencies [tagsDependency tags] $ do
      create [fromFilePath $ "tags/" ++ tag ++ ".xml"] $ do
        route   $ gsubRoute " " (const "-") `composeRoutes` setExtension "xml"
        compile $ loadAllSnapshots (fromList identifiers) "content"
          >>= fmap (take 10) . recentFirst
          >>= renderAtom (feedConfiguration $ Just tag) (bodyField "description" <> defaultContext)

  let pageRoute = gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"

  match ("pages/*" .&&. complement "pages/blog.markdown") $ version "nav-gen" $ do
    route   $ pageRoute
    compile $ pandocCompiler

  match "pages/blog.markdown" $ version "nav-gen" $ do
    route   $ constRoute "blog1.html"
    compile $ pandocCompiler

  paginateRules paginatedPosts $ \pageNum pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots pattern "content"

      navCtx <- genNavContext "pages/blog.markdown"

      let ctx = taggedPostCtx tags                                    <>
                paginateContext paginatedPosts pageNum                <>
                listField "posts" (taggedPostCtx tags) (return posts)
          indexCtx = navCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/pages/blog.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx

  match ("pages/*" .&&. complement "pages/blog.markdown") $ do
    route   $ pageRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/**" "content"

      -- Get the current Identifier
      curId <- getUnderlying

      let pageFilePath = toFilePath curId
          pageName     = takeBaseName pageFilePath
          recentPosts  = take 5 posts
          pageTemplate = "templates/pages/" ++ pageName ++ ".html"

      -- Generate navigation context
      navCtx <- genNavContext pageFilePath

      let masterCtx =
            listField "recentPosts" (taggedPostCtx tags) (return recentPosts) <>
            listField "posts" (taggedPostCtx tags) (return posts)             <>
            tagCloudField "tagCloud" 65 135 tags                              <>
            defaultContext
          indexCtx = navCtx

      sectionCtx <- getResourceBody >>= genSectionContext
      pg <- loadSnapshot (fromFilePath pageTemplate) "original"
        >>= applyAsTemplate (sectionCtx <> masterCtx)

      (makeItem . itemBody) pg
        >>= loadAndApplyTemplate "templates/default.html" indexCtx

  match "posts/**" $ do
    route   $ setExtension "html"
    compile $ do
      indexCtx <- genNavContext "pages/blog.markdown"

      pandocCompilerWith pandocReaderOptions pandocWriterOptions
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/partials/post.html" (taggedPostCtx tags)
        >>= loadAndApplyTemplate "templates/default.html" indexCtx

  create ["atom.xml"] $ do
    route   idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      blogPosts <- loadAllSnapshots ("posts/**" .&&. hasNoVersion) "content"
                     >>= fmap (take 10) . recentFirst
      renderAtom (feedConfiguration Nothing) feedCtx blogPosts

_devWatch :: IO ()
_devWatch = do
  _ <- hakyllWithExitCodeAndArgs myConfig (Options { verbosity = False, optCommand = Rebuild }) $ siteRules
  hakyllWithArgs myConfig (Options { verbosity = False, optCommand = Watch "localhost" 3000 False }) $ siteRules

main :: IO ()
main = hakyllWith myConfig siteRules

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

genNavContext :: String -> Compiler (Context String)
genNavContext ident = do
  pages <- sortByM pageWeight =<< loadAll ("pages/*" .&&. hasVersion "nav-gen")

  let (pagesFirst, pagesLast') = flip span pages $ \x ->
        (toFilePath . itemIdentifier $ x) /= ident
      pageMid = [head pagesLast']
      pagesLast = if not . null $ pagesLast' then tail pagesLast' else []

      indexCtx = listField "pagesFirst" defaultContext (return pagesFirst) <>
                 listField "pageMid"    defaultContext (return pageMid)    <>
                 listField "pagesLast"  defaultContext (return pagesLast)  <>
                 defaultContext
  return indexCtx

numPaginatePages :: Int
numPaginatePages = 6

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"   <>
          teaserField "teaser" "content" <>
          defaultContext

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = tagsField "tags" tags <> postCtx

pageWeight :: (Functor f, MonadMetadata f) => Item a -> f Int
pageWeight i = fmap (maybe 0 read) $ getMetadataField (itemIdentifier i) "weight"

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                 mapM (\x -> liftM (x,) (f x)) xs

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
