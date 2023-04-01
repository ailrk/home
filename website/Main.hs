module Main where


import Hakyll


main :: IO ()
main = hakyll $ do
  match "static/css/*.css"  $ do
    route idRoute
    compile copyFileCompiler

  match "static/images/**" $ do
    route idRoute
    compile copyFileCompiler
