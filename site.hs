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

  match (fromList ["contact.org", "cv.org", "skills.org", "projects.org"]) $ do
    route $ setExtension "html"
    compile $ customPandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ customPandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
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


  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts) `mappend`
            defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  let customExtensions = extensionsFromList [Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = defaultExtensions `mappend` customExtensions
      writerOptions = defaultHakyllWriterOptions
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = MathML
        }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions
