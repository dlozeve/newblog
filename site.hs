--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import           Data.Monoid (mappend)

import qualified Text.CSL                 as CSL
import           Text.CSL.Pandoc          (processCites)
import           Text.Pandoc
import           Text.Pandoc.Options
import           Text.Pandoc.Highlighting
import           Text.Pandoc.SideNote

import           Hakyll
import           Hakyll.Core.Compiler.Internal (compilerProvider, compilerAsk)
import           Hakyll.Core.Provider (resourceFilePath)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "favicon.ico" $ do
    route   idRoute
    compile copyFileCompiler

  match "files/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  match "css/et-book/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "bib/*" $ compile biblioCompiler

  match "csl/*" $ compile cslCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      underlying <- getUnderlying
      toc <- getMetadataField underlying "toc"
      customPandocCompiler (toc == Just "yes" || toc == Just "true")
      >>= return . fmap demoteHeaders
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match (fromList ["contact.org", "cv.org", "skills.org", "projects.org"]) $ do
    route $ setExtension "html"
    compile $ customPandocCompiler False
      >>= return . fmap demoteHeaders
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts) `mappend`
            constField "title" "Archives"            `mappend`
            defaultContext
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
            listField "posts" postCtx (return posts) `mappend`
            defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
  { feedTitle       = "Dimitri Lozeve's Blog"
  , feedDescription = "Recent posts"
  , feedAuthorName  = "Dimitri Lozeve"
  , feedAuthorEmail = "dimitri+web@lozeve.com"
  , feedRoot        = "https://www.lozeve.com"
  }

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

feedCtx :: Context String
feedCtx = postCtx <> bodyField "description"

-- Add links to references ------------------------------------------  
-- Source: https://github.com/jaspervdj/hakyll/issues/471#issuecomment-244540329
addLinkCitations (Pandoc meta a) =
  let prevMap = unMeta meta
      newMap = Map.insert "link-citations" (MetaBool True) prevMap
      newMeta = Meta newMap
  in  Pandoc newMeta a

myReadPandocBiblio :: ReaderOptions
                   -> Item CSL
                   -> Item Biblio
                   -> (Item String)
                   -> Compiler (Item Pandoc)
myReadPandocBiblio ropt csl biblio item = do
  -- Parse CSL file, if given
  provider <- compilerProvider <$> compilerAsk
  style <- unsafeCompiler $
           CSL.readCSLFile Nothing . (resourceFilePath provider) . itemIdentifier $ csl

  -- We need to know the citation keys, add then *before* actually parsing the
  -- actual page. If we don't do this, pandoc won't even consider them
  -- citations!
  let Biblio refs = itemBody biblio
  pandoc <- itemBody <$> readPandocWith ropt item
  let pandoc' = processCites style refs (addLinkCitations pandoc)

  return $ fmap (const pandoc') item

-- Pandoc compiler with maths, TOC, sidenots, and bibliography support --------------------
customPandocCompiler :: Bool -> Compiler (Item String)
customPandocCompiler withTOC =
  let customExtensions = extensionsFromList [Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = defaultExtensions `mappend` customExtensions
      writerOptions = defaultHakyllWriterOptions
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = MathJax ""
        }
      -- below copied from https://www.gwern.net/hakyll.hs
      -- below copied from https://github.com/jaspervdj/hakyll/blob/e8ed369edaae1808dffcc22d1c8fb1df7880e065/web/site.hs#L73 because god knows I don't know what this type bullshit is either:
      -- "When did it get so hard to compile a string to a Pandoc template?"
      tocTemplate =
        either error id $ either (error . show) id $
        runPure $ runWithDefaultPartials $
        compileTemplate "" "<div id=\"toc\"><h1>Table of Contents</h1>$toc$</div>\n$body$"
      writerOptionsWithTOC = writerOptions
        { writerTableOfContents = True
        , writerTOCDepth = 2
        , writerTemplate = Just tocTemplate--"<h1>Table of Contents</h1>$toc$\n$body$"
        }
      readerOptions = defaultHakyllReaderOptions
  in do
    csl <- load $ fromFilePath "csl/chicago-author-date.csl"
    bib <- load $ fromFilePath "bib/bibliography.bib"
    writePandocWith (if withTOC then writerOptionsWithTOC else writerOptions) <$>
     (getResourceBody >>= myReadPandocBiblio readerOptions csl bib >>= traverse (return . usingSideNotes))

type FeedRenderer = FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
  renderer feedConfiguration feedCtx
  =<< fmap (take 10) . recentFirst
  =<< loadAllSnapshots "posts/*" "content"
