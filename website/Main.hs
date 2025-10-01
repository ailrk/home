{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module Main where


import Hakyll (
    Configuration (destinationDirectory),
    Context (..),
    Identifier,
    Item (..),
    Tags,
    applyAsTemplate,
    buildTags,
    compile,
    compressCssCompiler,
    constField,
    copyFileCompiler,
    create,
    dateField,
    defaultConfiguration,
    defaultContext,
    demoteHeaders,
    escapeHtml,
    field,
    fromCapture,
    fromList,
    getResourceBody,
    getResourceLBS,
    hakyllWith,
    idRoute,
    listField,
    loadAll,
    loadAndApplyTemplate,
    makeItem,
    mapContext,
    match,
    modificationTimeField,
    pandocCompiler,
    recentFirst,
    relativizeUrls,
    renderTagList,
    route,
    saveSnapshot,
    setExtension,
    tagsField,
    tagsRules,
    templateCompiler,
    unixFilterLBS,
    (.||.), Compiler
 )
import Text.Regex.TDFA ((=~))
import Control.Arrow ((&&&))



main :: IO ()
main = hakyllWith defaultConfiguration{destinationDirectory = "docs"} do
    -- Static files
    match
        ( "images/**/*.jpg"
            .||. "images/**/*.png"
            .||. "images/**/*.gif"
            .||. "images/**/*.mp4"
            .||. "favicon.ico"
            .||. "files/**"
        )
        do
          route idRoute
          compile copyFileCompiler

    -- Index
    match "index.html" do
        route idRoute

        compile do
            tags <- buildTags "posts/*" (fromCapture "tags/*.html")
            posts <- recentFirst =<< loadAll "posts/*"
            let indexContext =
                    listField "posts" (postCtx tags) (return posts)
                        <> field "tags" (\_ -> renderTagList tags)
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/content.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

        -- Static files
        match
            ( "images/*.jpg"
                .||. "images/*.png"
                .||. "images/*.gif"
                .||. "images/*.mp4"
                .||. "favicon.ico"
                .||. "files/**"
            )
            do
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

        -- Build tags
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")

        -- Render each and every post
        match ("posts/*.md" .||. "posts/*.html" .||. "posts/*.lhs") do
            route $ setExtension ".html"
            compile do
                pandocCompiler
                    >>= saveSnapshot "content"
                    >>= return . fmap demoteHeaders
                    >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                    >>= loadAndApplyTemplate "templates/content.html" defaultContext
                    >>= pairA . (preloadImages &&& pure)
                    >>= \(preloads, body) -> loadAndApplyTemplate "templates/default.html" (defaultContext <> preloadField preloads) body
                    >>= relativizeUrls

        -- copy fonts
        match "fonts/*" do
            route idRoute
            compile copyFileCompiler

        -- Render some static pages
        match (fromList pages) do
            route $ setExtension ".html"
            compile $
                pandocCompiler
                    >>= loadAndApplyTemplate "templates/content.html" defaultContext
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

        -- Post list
        create ["posts.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let ctx =
                        constField "title" "Posts"
                            <> listField "posts" (postCtx tags) (return posts)
                            <> defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/content.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        -- Post tags
        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged " ++ tag

            -- Copied from posts, need to refactor
            route idRoute
            compile do
                posts <- recentFirst =<< loadAll pattern
                let ctx =
                        constField "title" title
                            <> listField "posts" (postCtx tags) (return posts)
                            <> defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/content.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        -- Read templates
        match "templates/*" $ compile templateCompiler

        -- Render the 404 page, we don't relativize URL's here.
        match "404.html" do
            route idRoute
            compile $
                pandocCompiler
                    >>= loadAndApplyTemplate "templates/content.html" defaultContext
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext


pages :: [Identifier]
pages = ["About.md"]


postCtx :: Tags -> Context String
postCtx tags =
    mconcat
        [ modificationTimeField "mtime" "%U"
        , dateField "date" "%B %e, %Y"
        , tagsField "tags" tags
        , Context $ \key -> case key of
            "title" -> unContext (mapContext escapeHtml defaultContext) key
            _ -> unContext mempty key
        , defaultContext
        ]


preloadField :: Item String -> Context a
preloadField preloads = field "preloads" (\_ -> pure $ itemBody preloads)


preloadImages :: Item String -> Compiler (Item String)
preloadImages html = makeItem preloads
  where
    preloads = unlines [ "<link rel=\"preload\" as=\"image\" href=\"" ++ url ++ "\">" | url <- imageUrls ]
    imageUrls =
      let regex = "<img[^>]+src=[\"']([^\"']+)[\"']" :: String
          matches = html' =~ regex :: [[String]]
      in map (!! 1) matches
    html' = itemBody html


pairA :: Applicative m => (m a, m b) -> m (a, b)
pairA (ma, mb) = (,) <$> ma <*> mb
