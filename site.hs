-- -*- coding: utf-8 -*-

{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortOn, isSuffixOf)
import Data.Ord (Down(..))
import Data.Monoid ((<>))

import Hakyll
import Text.Pandoc (Pandoc(..), Block(..), Inline(..), Format(..))
import Text.Pandoc.Walk (walk)

import Data.Git
import Data.Git.Revision

main :: IO ()
main = hakyll $ do
  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "js/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "fonts/**" $ do
    route   idRoute
    compile copyFileCompiler

  -- match "cv*.pdf" $ do
  --   route   idRoute
  --   compile copyFileCompiler

  match "PBougouliasCV.pdf" $ do
    route   idRoute
    compile copyFileCompiler

  match "less/**" $ 
    compile getResourceBody

  -- Compile the main less file
  -- We tell hakyll it depends on all the less files,
  -- so it will recompile it when needed
  d <- makePatternDependency "less/**"
  rulesExtraDependencies [d] $ create ["css/main.css"] $ do
    route idRoute
    compile $ loadBody "less/main.less"
      >>= makeItem
      >>= withItemBody (unixFilter "lessc" ["-x", "--include-path=less/", "-"])
  --                       [ "-x"
  --                       , "--include-path=less/"
  --                       , "--clean-css=--advanced --compatibility=ie8"
  --                       , "-"
  --                       ]

  match "css/**" $ do
    route   idRoute
    compile compressCssCompiler

  -- Compile recent publications
  match "recent-publications/*.md" $ do
    route $ setExtension "html"
    compile $ 
      pandocCompilerWithTransform 
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        externalLinks
         -- . noTopLevelParagraph
         -- . id

  -- Compile the index page
  match "index.html" $ do
    defCtx <- preprocess genDefCtx
    route idRoute
    compile $ do
      recentPubs <- mapM getInfo =<< loadAll "recent-publications/*.md"
      let idxCtx =
            listField "recent-publications" defCtx
              (return $ map fst $ sortOn reverseChronological recentPubs) <>
            defCtx
      getResourceBody
        >>= applyAsTemplate idxCtx
        >>= loadAndApplyTemplate "templates/default.html" defCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "templates/*" $ compile templateCompiler

getInfo item = do
  info <- getMetadata $ itemIdentifier item
  return (item, info)

reverseChronological (item, info) = Down $ lookupString "year" info

genDefCtx :: IO (Context String)
genDefCtx = do
  githash <- withCurrentRepo $ \g -> resolveRevision g (Revision "HEAD" [])
  return $ modificationTimeField "modified" "%d/%m/%Y, %H:%M %Z" <>
           constField "githash" (maybe "none" (take 7 . show) githash) <>
           defaultContext

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
  where idx = "index.html"
        clean url
          | idx `isSuffixOf` url = take (length url - length idx) url
          | otherwise            = url

externalLinks :: Pandoc -> Pandoc
externalLinks = walk fixLink
  where fixLink :: Inline -> Inline
        -- This is a quick hack, to be fixed later !!!
        fixLink (Link alt inlines (url, title)) = RawInline (Format "html") raw
          where raw = open ++ concatMap toRaw inlines ++ close
                toRaw (Str t) = t
                toRaw Space = " "
                open = "<a href=\"" ++ url ++ "\"" ++ t ++
                       " class=\"external\" target=\"_blank\">"
                t = if title == "" then "" else " title=\"" ++ title ++ "\""
                close = "</a>"
        fixLink inline = inline

noTopLevelParagraph :: Pandoc -> Pandoc
noTopLevelParagraph (Pandoc meta [Para inline]) = Pandoc meta [Plain inline]
noTopLevelParagraph p = p
