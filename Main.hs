{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Main where


import Hakyll
    ( copyFileCompiler, idRoute, compile, match, route, hakyllWith, defaultConfiguration, getResourceBody, relativizeUrls, (.||.), compressCssCompiler, create, loadAll, makeItem, Item (..), recentFirst, listField, applyAsTemplate, defaultContext, field, renderTagList, buildTags, fromCapture, Tags, Context(..), loadAndApplyTemplate, modificationTimeField, dateField, tagsField, mapContext, escapeHtml, pandocCompiler, templateCompiler, tagsRules, constField, setExtension, getResourceLBS, unixFilterLBS, saveSnapshot, demoteHeaders )


main :: IO ()
main = hakyllWith defaultConfiguration $ do


  -- Static files
  match ("images/*.jpg" .||. "images/*.png" .||. "images/*.gif" .||.
          "images/*.mp4" .||.
          "favicon.ico" .||. "files/**") $ do
      route   idRoute
      compile copyFileCompiler

  -- Index
  match "index.html" $ do
    route idRoute

    compile $ do
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")
        posts <- recentFirst =<< loadAll "posts/*"
        let indexContext =
                listField "posts" (postCtx tags) (return posts) <>
                field "tags" (\_ -> renderTagList tags) <>
                defaultContext

        getResourceBody
            >>= applyAsTemplate indexContext
            >>= loadAndApplyTemplate "templates/content.html" indexContext
            >>= loadAndApplyTemplate "templates/default.html" indexContext
            >>= relativizeUrls

    -- Static files
    match ("images/*.jpg" .||. "images/*.png" .||. "images/*.gif" .||.
            "images/*.mp4" .||.
            "favicon.ico" .||. "files/**") $ do
        route   idRoute
        compile copyFileCompiler

    -- Dot images
    match "images/*.dot" $ do
        route   $ setExtension "png"
        compile $ getResourceLBS >>= traverse (unixFilterLBS "dot" ["-Tpng"])

    -- Compress CSS into one file.
    match "css/*" $ compile compressCssCompiler
    create ["style.css"] $ do
        route idRoute
        compile $ do
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody csses

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match ("posts/*.md" .||. "posts/*.html" .||. "posts/*.lhs") $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/content.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = constField "title" "Posts" <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
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
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile $ templateCompiler


    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext


postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _       -> unContext mempty key
    , defaultContext
    ]
