--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import Data.Monoid (mappend)
import qualified Data.Text as T
import Hakyll
import Hakyll.Core.Compiler.Internal (compilerAsk, compilerProvider)
import Hakyll.Core.Provider (resourceFilePath)
import System.Directory (doesFileExist)
import System.FilePath (normalise, takeBaseName, takeDirectory, takeExtension, (</>))
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.SideNote (usingSideNotes)
import Text.Pandoc.Walk (walkM)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match (fromList ["favicon.ico", "favicon.svg", "favicon-96x96.png", "web-app-manifest-192x192.png", "web-app-manifest-512x512.png", "apple-touch-icon.png", "site.webmanifest"]) $ do
    route idRoute
    compile copyFileCompiler

  match "files/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "css/fonts/**" $ do
    route idRoute
    compile copyFileCompiler

  match "bib/*.bib" $ compile biblioCompiler

  match "bib/*.csl" $ compile cslCompiler

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx =
            constField "title" title
              <> listField "posts" (postCtxWithTags tags) (return posts)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      do
        underlying <- getUnderlying
        toc <- getMetadataField underlying "toc"
        customPandocCompiler (toc == Just "yes" || toc == Just "true")
        >>= return . fmap demoteHeaders
        >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
        >>= relativizeUrls

  match (fromList ["contact.org", "cv.org", "skills.org", "projects.org"]) $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler False
        >>= return . fmap demoteHeaders
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile (feedCompiler renderAtom)

  create ["rss.xml"] $ do
    route idRoute
    compile (feedCompiler renderRss)

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) $ recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  match "404.html" $ do
    route idRoute
    compile $
      customPandocCompiler False
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "Dimitri Lozeve's Blog",
      feedDescription = "Recent posts",
      feedAuthorName = "Dimitri Lozeve",
      feedAuthorEmail = "dimitri+web@lozeve.com",
      feedRoot = "https://www.lozeve.com"
    }

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

feedCtx :: Context String
feedCtx = postCtx <> bodyField "description"

-- Pandoc compiler with maths, TOC, sidenotes, and bibliography support --------------------
customPandocCompiler :: Bool -> Compiler (Item String)
customPandocCompiler withTOC =
  let customExtensions = extensionsFromList [Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = defaultExtensions `mappend` customExtensions
      writerOptions =
        defaultHakyllWriterOptions
          { writerExtensions = newExtensions,
            writerHTMLMathMethod = MathJax ""
          }
      -- below copied from https://www.gwern.net/hakyll.hs
      -- below copied from https://github.com/jaspervdj/hakyll/blob/e8ed369edaae1808dffcc22d1c8fb1df7880e065/web/site.hs#L73 because god knows I don't know what this type bullshit is either:
      -- "When did it get so hard to compile a string to a Pandoc template?"
      tocTemplate =
        either error id $
          either (error . show) id $
            runPure $
              runWithDefaultPartials $
                compileTemplate "" "<div id=\"toc\"><h1>Table of Contents</h1>$toc$</div>\n$body$"
      writerOptionsWithTOC =
        writerOptions
          { writerTableOfContents = True,
            writerTOCDepth = 2,
            writerTemplate = Just tocTemplate -- "<h1>Table of Contents</h1>$toc$\n$body$"
          }
      readerOptions = defaultHakyllReaderOptions
   in do
        csl <- load $ fromFilePath "bib/chicago-author-date.csl"
        bib <- load $ fromFilePath "bib/bibliography.bib"
        writePandocWith (if withTOC then writerOptionsWithTOC else writerOptions)
          <$> ( getResourceBody
                  >>= readPandocBiblio readerOptions csl bib
                  >>= traverse (return . usingSideNotes)
                  >>= traverse darkModeImages
              )

-- | Replace images with <picture> elements when a "-dark" variant exists on disk.
-- e.g. images/foo.png will use images/foo-dark.png for dark mode if that file exists.
-- Resolves relative paths (like ../images/foo.png) from the source file's directory.
darkModeImages :: Pandoc -> Compiler Pandoc
darkModeImages doc = do
  srcPath <- getResourceFilePath
  let srcDir = takeDirectory srcPath
  walkM (processInline srcDir) doc
  where
    processInline srcDir img@(Image _attr inlines (url, _title))
      | not ("http" `T.isPrefixOf` url) = do
          let urlStr = T.unpack url
              -- Resolve relative to the source file's directory
              resolved = normalise (srcDir </> urlStr)
              dir = takeDirectory resolved
              base = takeBaseName resolved
              ext = takeExtension resolved
              darkPath = dir </> base ++ "-dark" ++ ext
              -- Build the dark URL with the same relativity as the original
              urlDir = takeDirectory urlStr
              urlBase = takeBaseName urlStr
              urlExt = takeExtension urlStr
              darkUrl = urlDir </> urlBase ++ "-dark" ++ urlExt
              alt = T.unpack (stringify inlines)
          darkExists <- unsafeCompiler $ doesFileExist darkPath
          if darkExists
            then
              return $
                RawInline "html" $
                  T.pack $
                    concat
                      [ "<picture>",
                        "<source srcset=\"", darkUrl, "\" media=\"(prefers-color-scheme: dark)\">",
                        "<img src=\"", urlStr, "\" alt=\"", alt, "\">",
                        "</picture>"
                      ]
            else return img
    processInline _ x = return x

type FeedRenderer =
  FeedConfiguration ->
  Context String ->
  [Item String] ->
  Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
  renderer feedConfiguration feedCtx
    =<< fmap (take 10) . recentFirst
    =<< loadAllSnapshots "posts/*" "content"
