{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module Main where

import Hakyll
import System.FilePath (takeBaseName, (<.>), (</>))
import Text.Pandoc (HTMLMathMethod (..), WriterOptions (..))
import Text.Regex.TDFA ((=~))

--------------------------------------------------------------------------------
-- Sections
--
-- One site, many pseudo homes:
--
--   /                 generic info about me (no posts)
--   /home/en/          general posts, English
--   /home/cn/          general posts, Chinese
--   /code/en/         tech posts, English
--   /code/cn/         tech posts, Chinese
--
-- Sources live in posts/<kind>/<lang>/*.md and are routed to
-- <kind>/<lang>/posts/<name>.html.
--
-- To add a new sub blog, add a constructor to `Kind` or `Lang` and
-- create the matching posts/<kind>/<lang>/ folder. Nothing else changes.
--------------------------------------------------------------------------------

data Kind = Yap | Code deriving (Eq, Show, Enum, Bounded)

data Lang = En | Cn deriving (Eq, Show, Enum, Bounded)

data Section = Section {secKind :: Kind, secLang :: Lang}
    deriving (Eq, Show)

sections :: [Section]
sections = [Section k l | k <- [minBound ..], l <- [minBound ..]]

kindSlug :: Kind -> String
kindSlug Yap = "home"
kindSlug Code = "code"

langSlug :: Lang -> String
langSlug En = "en"
langSlug Cn = "cn"

htmlLang :: Lang -> String
htmlLang En = "en"
htmlLang Cn = "zh"

otherLang :: Lang -> Lang
otherLang En = Cn
otherLang Cn = En

otherKind :: Kind -> Kind
otherKind Yap = Code
otherKind Code = Yap

-- "home/en"
secDir :: Section -> FilePath
secDir (Section k l) = kindSlug k </> langSlug l

-- "/home/en/"
secUrl :: Section -> String
secUrl s = "/" ++ secDir s ++ "/"

-- "posts/home/en"
srcDir :: Section -> FilePath
srcDir s = "posts" </> secDir s

postsPattern :: Section -> Pattern
postsPattern s =
    fromGlob (srcDir s </> "*.md")
        .||. fromGlob (srcDir s </> "*.html")
        .||. fromGlob (srcDir s </> "*.lhs")

-- Per language UI strings.
data Strings = Strings
    { sHome :: String
    , sPosts :: String
    , sTags :: String
    , sAllTags :: String
    , sTagged :: String -> String
    , sDate :: String
    }

strings :: Lang -> Strings
strings En =
    Strings
        { sHome = "Home"
        , sPosts = "Posts"
        , sTags = "Tags"
        , sAllTags = "All Tags"
        , sTagged = \t -> "Posts tagged " ++ t
        , sDate = "%B %e, %Y"
        }
strings Cn =
    Strings
        { sHome = "首页"
        , sPosts = "文章"
        , sTags = "标签"
        , sAllTags = "全部标签"
        , sTagged = \t -> "标签：" ++ t
        , sDate = "%Y年%m月%d日"
        }

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

-- Fields every template in a section can use:
--   $home$ $root$ $postsUrl$ $tagsUrl$ $lang$ $htmlLang$ $sectionName$
--   $navHome$ $navPosts$ $navTags$
--   $otherLangUrl$ $otherLangLabel$ $otherKindUrl$ $otherKindLabel$
secCtx :: Section -> Context a
secCtx s@(Section k l) =
    mconcat
        [ constField "home" (secUrl s)
        , constField "root" "/"
        , constField "postsUrl" (secUrl s ++ "posts.html")
        , constField "tagsUrl" (secUrl s ++ "tags.html")
        , constField "lang" (langSlug l)
        , constField "htmlLang" (htmlLang l)
        , constField "sectionName" (kindSlug k ++ " / " ++ langSlug l)
        , constField "navHome" (st.sHome)
        , constField "navPosts" (st.sPosts)
        , constField "navTags" (st.sTags)
        , constField "allTags" (st.sAllTags)
        , constField "otherLangUrl" (secUrl (Section k (otherLang l)))
        , constField "otherLangLabel" (langLabel (otherLang l))
        , constField "otherKindUrl" (secUrl (Section (otherKind k) l))
        , constField "otherKindLabel" (kindSlug (otherKind k))
        , constField "feedUrl" (secUrl s ++ "atom.xml")
        ]
  where
    st = strings l
    langLabel En = "English"
    langLabel Cn = "中文"

-- Context for pages that live at the root (/, About, 404).
rootCtx :: Context String
rootCtx =
    constField "home" "/"
        <> constField "root" "/"
        <> constField "htmlLang" "en"
        <> defaultContext

-- Everything a page in a section needs.
baseCtx :: Section -> Context String
baseCtx s = secCtx s <> defaultContext

postCtx :: Section -> Tags -> Context String
postCtx s tags =
    mconcat
        [ modificationTimeField "mtime" "%U"
        , dateField "date" ((strings s.secLang).sDate)
        , tagsField "tags" tags
        , Context $ \key -> case key of
            "title" -> unContext (mapContext escapeHtml defaultContext) key
            _ -> unContext mempty key
        , baseCtx s
        ]

-- Context representing an individual tag item
tagCtx :: Tags -> Context (String, [Identifier])
tagCtx tags =
    field "tag" (return . fst . itemBody)
        <> field
            "url"
            ( \item -> do
                let tag = fst (itemBody item)
                getRoute (tagsMakeId tags tag) >>= \case
                    Just url -> return (toUrl url)
                    Nothing -> fail $ "No route found for tag: " ++ tag
            )

-- Context with a list of all tags of one section
allTagsCtx :: Section -> Tags -> Context String
allTagsCtx s tags =
    listField "tags" (tagCtx tags) (mapM makeItem $ tagsMap tags)
        <> baseCtx s

--------------------------------------------------------------------------------
-- Math / preload helpers
--------------------------------------------------------------------------------

mathPandocCompiler :: Compiler (Item String)
mathPandocCompiler =
    pandocCompilerWith
        defaultHakyllReaderOptions
        (defaultHakyllWriterOptions{writerHTMLMathMethod = MathJax ""})

preloadField :: Item String -> Context a
preloadField preloads = field "preloads" (\_ -> pure $ itemBody preloads)

preloadImages :: Item String -> Compiler (Item String)
preloadImages html = makeItem preloads
  where
    preloads =
        unlines
            [ "<link rel=\"preload\" as=\"image\" href=\"" ++ url ++ "\">"
            | url <- imageUrls
            ]
    imageUrls =
        let regex = "<img[^>]+src=[\"']([^\"']+)[\"']" :: String
         in map (!! 1) (itemBody html =~ regex :: [[String]])

--------------------------------------------------------------------------------
-- Feeds
--------------------------------------------------------------------------------

feedConfig :: Section -> FeedConfiguration
feedConfig (Section k l) =
    FeedConfiguration
        { feedTitle = "Ailrk · " ++ kindSlug k ++ " / " ++ langSlug l
        , feedDescription = "Ailrk's blog, " ++ kindSlug k ++ ", " ++ langSlug l
        , feedAuthorName = "ailrk"
        , feedAuthorEmail = "jimmy123good@gmail.com"  -- change me
        , feedRoot = "https://blog.ailrk.com"
        }

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

imagesFolder :: Pattern
imagesFolder =
    "images/*.jpg"
        .||. "images/*.jpeg"
        .||. "images/*.png"
        .||. "images/*.webp"
        .||. "images/*.gif"
        .||. "images/*.mp4"

-- Root level pages, these are not part of any section.
pages :: [Identifier]
pages = ["About.md"]

main :: IO ()
main = hakyllWith defaultConfiguration{destinationDirectory = "dist"} do
    -- Static files
    match (imagesFolder .||. "favicon.ico" .||. "files/**") do
        route idRoute
        compile copyFileCompiler

    match "fonts/*" do
        route idRoute
        compile copyFileCompiler

    -- Dot images
    match "images/*.dot" do
        route $ setExtension "png"
        compile $ getResourceLBS >>= traverse (unixFilterLBS "dot" ["-Tpng"])

    -- Compress CSS into one file.
    match "css/*" $ compile compressCssCompiler
    create ["style.css"] do
        route idRoute
        compile do
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody csses

    -- Templates
    match "templates/*" $ compile templateCompiler

    -- Root: generic info about me only. No posts here.
    match "index.html" do
        route idRoute
        compile $
          getResourceBody
            >>= applyAsTemplate rootCtx
            >>= loadAndApplyTemplate "templates/content.html" rootCtx
            >>= loadAndApplyTemplate "templates/default.html" rootCtx
            >>= relativizeUrls

    match (fromList pages) do
        route $ setExtension ".html"
        compile $
          mathPandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" rootCtx
            >>= loadAndApplyTemplate "templates/default.html" rootCtx
            >>= relativizeUrls

    -- Every pseudo home
    mapM_ sectionRules sections

    -- 404, we don't relativize URL's here.
    match "404.html" do
      route idRoute
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/content.html" rootCtx
          >>= loadAndApplyTemplate "templates/default.html" rootCtx

-- All the rules for one pseudo home.
sectionRules :: Section -> Rules ()
sectionRules s = do
    let st = strings s.secLang
        dir = secDir s
        pat = postsPattern s
        ctx = baseCtx s

    -- Tags are scoped to this section only.
    tags <- buildTags pat (fromCapture (fromGlob (dir </> "tags/*.html")))

    -- Posts
    match pat do
      route $ customRoute \i ->
        dir </> "posts" </> takeBaseName (toFilePath i) <.> "html"
      compile do
        body <-
            mathPandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
        preloads <- preloadImages body
        loadAndApplyTemplate "templates/post.html" (postCtx s tags) body
          >>= loadAndApplyTemplate "templates/content.html" ctx
          >>= loadAndApplyTemplate
              "templates/default.html"
              (ctx <> preloadField preloads)
          >>= relativizeUrls

    -- Section home: <dir>/index.html
    create [fromFilePath (dir </> "index.html")] do
      route idRoute
      compile $
        listing s tags "templates/section-home.html" st.sHome pat

    -- Post list: <dir>/posts.html
    create [fromFilePath (dir </> "posts.html")] do
      route idRoute
      compile $
        listing s tags "templates/posts.html" st.sPosts pat

    -- All tags: <dir>/tags.html
    create [fromFilePath (dir </> "tags.html")] do
      route idRoute
      compile do
        let tctx = constField "title" st.sAllTags <> allTagsCtx s tags
        makeItem ""
          >>= loadAndApplyTemplate "templates/tags-list.html" tctx
          >>= loadAndApplyTemplate "templates/default.html" tctx
          >>= relativizeUrls

    -- One page per tag: <dir>/tags/<tag>.html
    tagsRules tags \tag tagPat -> do
      route idRoute
      compile $
        listing s tags "templates/posts.html" (st.sTagged tag) tagPat

    -- Atom feed: <dir>/atom.xml
    create [fromFilePath (dir </> "atom.xml")] do
        route idRoute
        compile do
            let fctx = bodyField "description" <> defaultContext
            posts <-
                fmap (take 20) . recentFirst
                    =<< loadAllSnapshots pat "content"
            renderAtom (feedConfig s) fctx posts

-- Render a page that lists posts, newest first.
listing ::
    Section ->
    Tags ->
    Identifier ->
    String ->
    Pattern ->
    Compiler (Item String)
listing s tags template title pat = do
    posts <- recentFirst =<< loadAll pat
    let ctx =
            constField "title" title
                <> listField "posts" (postCtx s tags) (return posts)
                <> baseCtx s
    makeItem ""
        >>= loadAndApplyTemplate template ctx
        >>= loadAndApplyTemplate "templates/content.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
