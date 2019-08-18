--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)

import           Text.Pandoc.Options
import           Text.Pandoc.Highlighting

import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "files/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "bib/*" $ compile biblioCompiler

  match "csl/*" $ compile cslCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ customPandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "projects/*" $ do
    route $ setExtension "html"
    compile $ customPandocCompiler
      >>= loadAndApplyTemplate "templates/project.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match (fromList ["contact.org", "cv.org", "skills.org", "projects.org"]) $ do
    route $ setExtension "html"
    compile $ customPandocCompiler
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

  create ["projects.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "projects/*"
      let archiveCtx =
            listField "projects" postCtx (return posts) `mappend`
            constField "title" "Projects"               `mappend`
            defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/projects.html" archiveCtx
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

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  let customExtensions = extensionsFromList [Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = defaultExtensions `mappend` customExtensions
      writerOptions = defaultHakyllWriterOptions
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = KaTeX ""
        }
      readerOptions = defaultHakyllReaderOptions
  in do
    csl <- load $ fromFilePath "csl/chicago-author-date.csl"
    bib <- load $ fromFilePath "bib/all.bib"
    writePandocWith writerOptions <$>
     (getResourceBody >>= readPandocBiblio readerOptions csl bib)

type FeedRenderer = FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
  renderer feedConfiguration feedCtx
  =<< fmap (take 10) . recentFirst
  =<< loadAllSnapshots "posts/*" "content"
