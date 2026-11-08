--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (throwIO)
import Data.Char (isAlphaNum, ord)
import Data.List (intercalate, isPrefixOf)
import Data.Monoid (Any (..))
import qualified Data.Text as T
import Hakyll
import System.Directory (doesFileExist)
import System.FilePath (normalise, splitDirectories, takeBaseName, takeDirectory, takeExtension, takeFileName, (</>))
import Text.Pandoc hiding (Template)
import Text.Pandoc.Filter (Environment (..), Filter (..), applyFilters)
import Text.Pandoc.Lua (getEngine)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query, walk, walkM)
import Text.Printf (printf)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "well-known/*" $ do
    route $ gsubRoute "well-known/" (const ".well-known/")
    compile copyFileCompiler

  match "_headers" $ do
    route idRoute
    compile copyFileCompiler

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
              <> baseCtx

      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= return . fmap demoteHeaders
        >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
        >>= relativizeUrls

  match (fromList ["contact.org", "cv.org", "projects.org"]) $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= return . fmap demoteHeaders
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" baseCtx
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Archives"
              <> baseCtx
      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile (feedCompiler renderAtomWithTemplates "templates/atom.xml" "templates/atom-item.xml")

  create ["rss.xml"] $ do
    route idRoute
    compile (feedCompiler renderRssWithTemplates "templates/rss.xml" "templates/rss-item.xml")

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      singlePages <-
        loadAll
          (fromList ["index.html", "archive.html", "projects.org", "cv.org", "contact.org"])
      let sitemapCtx =
            listField
              "posts"
              (dateField "isodate" "%Y-%m-%d" <> canonicalCtx <> defaultContext)
              (return posts)
              <> listField "pages" (canonicalCtx <> defaultContext) (return singlePages)
      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  match "robots.txt" $ do
    route idRoute
    compile copyFileCompiler

  create ["llms.txt"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let llmsCtx =
            listField "posts" postCtx (return posts)
              <> constField "siteRoot" siteRoot
      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/llms.txt" llmsCtx

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) $ recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> constField "homepage" "true"
              <> baseCtx
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  match "404.html" $ do
    route idRoute
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" baseCtx

--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "Dimitri Lozeve's Blog",
      feedDescription = "Recent posts",
      feedAuthorName = "Dimitri Lozeve",
      feedAuthorEmail = "dimitri+web@lozeve.com",
      feedRoot = siteRoot
    }

-- | Canonical origin for the site. Non-www is the canonical host.
siteRoot :: String
siteRoot = "https://lozeve.com"

-- | Absolute canonical URL for an item, stripping any trailing "index.html".
canonicalCtx :: Context String
canonicalCtx = field "canonical" $ \item -> do
  mbRoute <- getRoute (itemIdentifier item)
  return $ case mbRoute of
    Nothing -> siteRoot ++ "/"
    Just r -> siteRoot ++ toUrl (stripIndex r)
  where
    stripIndex r
      | takeFileName r == "index.html" =
          case takeDirectory r of
            "." -> ""
            d -> d ++ "/"
      | otherwise = r

-- | Default context plus the canonical URL field.
baseCtx :: Context String
baseCtx = canonicalCtx <> defaultContext <> constField "siteRoot" siteRoot

-- | Minimal JSON string escaping for values interpolated into JSON-LD.
escapeJson :: String -> String
escapeJson = concatMap esc
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    -- These are safe in JSON but can terminate the surrounding <script> tag.
    esc '<' = "\\u003c"
    esc '>' = "\\u003e"
    esc '&' = "\\u0026"
    esc c
      | c < ' ' = printf "\\u%04x" (ord c)
      | otherwise = [c]

-- | Expose a metadata field, JSON-escaped, under a new name (fails if absent).
jsonField :: String -> String -> Context String
jsonField name metaKey = field name $ \item -> do
  mv <- getMetadataField (itemIdentifier item) metaKey
  maybe (noResult ("no metadata field " ++ metaKey)) (return . escapeJson) mv

-- | JSON-escaped fields used inside the JSON-LD structured data block.
jsonCtx :: Context String
jsonCtx =
  jsonField "title_json" "title"
    <> jsonField "description_json" "description"

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> dateField "isodate" "%Y-%m-%d"
    <> constField "author" "Dimitri Lozeve"
    <> canonicalCtx
    <> defaultContext
    <> jsonCtx
    <> constField "siteRoot" siteRoot

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

-- | The full post body with its URLs resolved to absolute URLs, so that the
-- content renders correctly inside feed readers. Relative URLs are resolved
-- against the post's own route (e.g. @../images/foo.png@ in @posts/bar.org@
-- becomes @https://lozeve.com/images/foo.png@).
feedContent :: Context String
feedContent = field "content" $ \item -> do
  mbRoute <- getRoute (itemIdentifier item)
  let routeDir = maybe "" takeDirectory mbRoute
  return $ withUrls (toAbsolute routeDir) (itemBody item)
  where
    toAbsolute routeDir url
      | isExternal url = url
      | hasUrlScheme url = url
      | "#" `isPrefixOf` url = url
      | "/" `isPrefixOf` url = siteRoot ++ url
      | otherwise = siteRoot ++ "/" ++ resolveUrl routeDir url

-- | Resolve a relative URL against a base directory, collapsing "." and "..".
resolveUrl :: FilePath -> FilePath -> FilePath
resolveUrl base rel =
  intercalate "/" $ foldl step [] (splitDirectories base ++ splitDirectories rel)
  where
    step acc "." = acc
    step acc ".." = dropLast acc
    step acc seg = acc ++ [seg]
    dropLast [] = []
    dropLast xs = init xs

feedCtx :: Context String
feedCtx = postCtx <> feedContent

-- | Writer options shared by the Pandoc writer and the Lua filters, so that
-- filters see the same @PANDOC_WRITER_OPTIONS@ as the final rendering.
pandocWriterOptions :: WriterOptions
pandocWriterOptions =
  defaultHakyllWriterOptions
    { writerExtensions =
        writerExtensions defaultHakyllWriterOptions
          <> extensionsFromList [Ext_latex_macros],
      writerMathMethod = MathML
    }

-- Pandoc compiler with maths, TOC, sidenotes, and bibliography support --------------------
customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  let -- below copied from https://www.gwern.net/hakyll.hs
      -- below copied from https://github.com/jaspervdj/hakyll/blob/e8ed369edaae1808dffcc22d1c8fb1df7880e065/web/site.hs#L73 because god knows I don't know what this type bullshit is either:
      -- "When did it get so hard to compile a string to a Pandoc template?"
      compileTpl s =
        either error id $
          either (error . show) id $
            runPure $
              runWithDefaultPartials $
                compileTemplate "" s
      tocOnlyTemplate = compileTpl "<div id=\"toc\"><h1>Table of Contents</h1>$toc$</div>"
      writerOptionsTocOnly =
        pandocWriterOptions
          { writerTableOfContents = True,
            writerTOCDepth = 2,
            writerTemplate = Just tocOnlyTemplate
          }
      readerOptions = defaultHakyllReaderOptions
   in do
        csl <- load $ fromFilePath "bib/chicago-author-date.csl"
        bib <- load $ fromFilePath "bib/bibliography.bib"
        doc <-
          getResourceBody
            >>= readPandocBiblio readerOptions csl bib
            >>= traverse sidenoteCompiler
            >>= traverse numberedEnvsCompiler
            >>= traverse darkModeImages
        -- A @#+TOC:@ marker places the TOC at that spot in the post, which
        -- allows e.g. an introductory paragraph before it. Without a marker, no
        -- table of contents is generated.
        return $
          writePandocWith pandocWriterOptions $
            if hasTocMarker (itemBody doc)
              then insertToc writerOptionsTocOnly doc
              else doc

-- | The table of contents can be placed explicitly in a post with Org's
-- @#+TOC:@ keyword (case-insensitive), written where it should appear.
hasTocMarker :: Pandoc -> Bool
hasTocMarker = getAny . query (Any . isMarker)
  where
    isMarker (RawBlock (Format "org") s) = isTocKeyword s
    isMarker _ = False

-- | Whether a raw Org block is a @#+TOC:@ keyword. Case-insensitive, and any
-- trailing arguments (e.g. @#+TOC: headlines 2@) are accepted.
isTocKeyword :: T.Text -> Bool
isTocKeyword = T.isPrefixOf "#+TOC:" . T.toUpper . T.stripStart

-- | Replace the TOC marker with the rendered table of contents, computed from
-- the document's headings with the given writer options.
insertToc :: WriterOptions -> Item Pandoc -> Item Pandoc
insertToc tocOptions doc =
  let tocHtml = T.pack $ itemBody $ writePandocWith tocOptions doc
   in fmap (walk (replaceWith tocHtml)) doc
  where
    replaceWith html (RawBlock (Format "org") s)
      | isTocKeyword s = RawBlock "html" html
    replaceWith _ block = block

-- | Apply a single Lua filter to a Pandoc document.
applyLuaFilter :: FilePath -> Pandoc -> Compiler Pandoc
applyLuaFilter path doc = unsafeCompiler $ do
  engine <- getEngine
  let env =
        Environment
          { envReaderOptions = defaultHakyllReaderOptions,
            envWriterOptions = pandocWriterOptions
          }
  runIO (applyFilters engine env [LuaFilter path] ["html"] doc)
    >>= either throwIO pure

-- | Path to the vendored pandoc-sidenote Lua filter, relative to the project
-- root. Downloaded from https://github.com/jez/pandoc-sidenote.
sidenoteFilterPath :: FilePath
sidenoteFilterPath = "filters/pandoc-sidenote.lua"

-- | Convert Pandoc footnotes into Tufte-style sidenotes using the
-- pandoc-sidenote Lua filter.
sidenoteCompiler :: Pandoc -> Compiler Pandoc
sidenoteCompiler = applyLuaFilter sidenoteFilterPath

-- | Path to the Lua filter that numbers theorem-like environments and resolves
-- references to them.
numberedEnvsFilterPath :: FilePath
numberedEnvsFilterPath = "filters/pandoc-numbered-envs.lua"

-- | Number definitions, propositions, etc. and resolve cross-references.
numberedEnvsCompiler :: Pandoc -> Compiler Pandoc
numberedEnvsCompiler = applyLuaFilter numberedEnvsFilterPath

-- | Whether a URL starts with a URI scheme (e.g. "mailto:", "data:").
hasUrlScheme :: String -> Bool
hasUrlScheme s =
  case span (/= ':') s of
    (scheme, ':' : _) -> not (null scheme) && all isSchemeChar scheme
    _ -> False
  where
    isSchemeChar c = isAlphaNum c || c `elem` ("+-." :: String)

-- | Replace images with <picture> elements when a "-dark" variant exists on disk.
-- e.g. images/foo.png will use images/foo-dark.png for dark mode if that file exists.
-- Resolves relative paths (like ../images/foo.png) from the source file's directory.
darkModeImages :: Pandoc -> Compiler Pandoc
darkModeImages doc = do
  srcPath <- getResourceFilePath
  let srcDir = takeDirectory srcPath
  walkM (processInline srcDir) doc
  where
    processInline srcDir img@(Image (ident, classes, kvs) inlines (url, title))
      | not (isExternal (T.unpack url)) && not (hasUrlScheme (T.unpack url)) = do
          let urlStr = T.unpack url
              -- Resolve paths relative to the source file's directory, unless
              -- the image is already rooted at the site root.
              resolved
                | "/" `isPrefixOf` urlStr = normalise (drop 1 urlStr)
                | otherwise = normalise (srcDir </> urlStr)
              dir = takeDirectory resolved
              base = takeBaseName resolved
              ext = takeExtension resolved
              darkPath = dir </> base ++ "-dark" ++ ext
              -- Build the dark URL with the same relativity as the original
              urlDir = takeDirectory urlStr
              urlBase = takeBaseName urlStr
              urlExt = takeExtension urlStr
              darkUrl = urlDir </> urlBase ++ "-dark" ++ urlExt
              -- Alt text comes from the image description (inlines) or, when set
              -- via "#+ATTR_HTML: :alt ...", from the "alt" attribute key.
              alt = case T.unpack (stringify inlines) of
                "" -> maybe "" T.unpack (lookup "alt" kvs)
                s -> s
              titleText = case lookup "title" kvs of
                Just t | T.null title -> t
                _ -> title
              attrs =
                [("src", urlStr), ("alt", alt)]
                  ++ [("title", T.unpack titleText) | not (T.null titleText)]
                  ++ [("id", T.unpack ident) | not (T.null ident)]
                  ++ [("class", T.unpack (T.intercalate " " classes)) | not (null classes)]
                  ++ [ (T.unpack k, T.unpack v)
                       | (k, v) <- kvs,
                         k `notElem` ["alt", "src", "title", "id", "class"]
                     ]
              renderAttr (k, v) = k ++ "=\"" ++ escapeHtml v ++ "\""
          darkExists <- unsafeCompiler $ doesFileExist darkPath
          if darkExists
            then
              return $
                RawInline "html" $
                  T.pack $
                    concat
                      [ "<picture>",
                        "<source srcset=\"", escapeHtml darkUrl, "\" media=\"(prefers-color-scheme: dark)\">",
                        "<img ", unwords (map renderAttr attrs), ">",
                        "</picture>"
                      ]
            else return img
    processInline _ x = return x

type FeedRenderer =
  Template ->
  Template ->
  FeedConfiguration ->
  Context String ->
  [Item String] ->
  Compiler (Item String)

feedCompiler :: FeedRenderer -> Identifier -> Identifier -> Compiler (Item String)
feedCompiler renderer feedTemplateId itemTemplateId = do
  feedTemplate <- itemBody <$> load feedTemplateId
  itemTemplate <- itemBody <$> load itemTemplateId
  items <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
  renderer feedTemplate itemTemplate feedConfiguration feedCtx items
