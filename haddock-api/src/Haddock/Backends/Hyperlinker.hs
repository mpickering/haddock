module Haddock.Backends.Hyperlinker
    ( ppHyperlinkedSource
    , module Haddock.Backends.Hyperlinker.Types
    , module Haddock.Backends.Hyperlinker.Utils
    ) where


import Haddock.Types
import Haddock.Backends.Hyperlinker.Renderer
import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Utils

import Text.XHtml hiding ((</>))

import Data.Maybe
import System.Directory
import System.FilePath
import qualified GHC
import qualified Name as GHC
import qualified OccName as GHC

import Data.Ord
import Data.List
import Data.Function


-- | Generate hyperlinked source for given interfaces.
--
-- Note that list of interfaces should also contain interfaces normally hidden
-- when generating documentation. Otherwise this could lead to dead links in
-- produced source.
ppHyperlinkedSource :: FilePath -- ^ Output directory
                    -> FilePath -- ^ Resource directory
                    -> Maybe FilePath -- ^ Custom CSS file path
                    -> Bool -- ^ Flag indicating whether to pretty-print HTML
                    -> SrcMap -- ^ Paths to sources
                    -> [Interface] -- ^ Interfaces for which we create source
                    -> IO ()
ppHyperlinkedSource outdir libdir mstyle pretty srcs ifaces = do
    createDirectoryIfMissing True srcdir
    let cssFile = fromMaybe (defaultCssFile libdir) mstyle
    copyFile cssFile $ srcdir </> srcCssFile
    copyFile (libdir </> "html" </> highlightScript) $
        srcdir </> highlightScript
    print (findUnusedIdentifiers (combineUsages ifaces))
    mapM_ (ppHyperlinkedModuleSource srcdir pretty srcs) ifaces
  where
    srcdir = outdir </> hypSrcDir

findUnusedIdentifiers :: [TokenDetails] -> [GHC.Name]
findUnusedIdentifiers rs = mapMaybe process . chunk
                         . sortBy (comparing (fromLeft . rtkName)) $ noModNames
  where
    noModNames = filter (not . isModName) rs
    isModName (RtkModule m) = True
    isModName _ = False

    fromLeft (Left x) = x

    chunk = groupBy ((==) `on` rtkName)


    process :: [TokenDetails] -> Maybe GHC.Name
    process ts@(t:_)
      | not hasBind = Nothing
      | not (GHC.isVarNameSpace namespace) = Nothing
      | otherwise =  foldr go (Just name) ts
      where
        hasBind = RtkDecl name `elem` ts
        name = case rtkName t of { Left x -> x }

        namespace = GHC.occNameSpace (GHC.occName name)

        go (RtkVar _) _ = Nothing
        go _ t = t

combineUsages :: [Interface] -> [TokenDetails]
combineUsages ifaces = mapMaybe rtkDetails (concat $ mapMaybe ifaceTokenizedSrc ifaces)

-- | Generate hyperlinked source for particular interface.
ppHyperlinkedModuleSource :: FilePath -> Bool -> SrcMap -> Interface
                          -> IO ()
ppHyperlinkedModuleSource srcdir pretty srcs iface =
    case ifaceTokenizedSrc iface of
        Just tokens -> writeFile path . html . render' $ tokens
        Nothing -> return ()
  where
    render' = render (Just srcCssFile) (Just highlightScript) srcs
    html = if pretty then renderHtml else showHtml
    path = srcdir </> hypSrcModuleFile (ifaceMod iface)

-- | Name of CSS file in output directory.
srcCssFile :: FilePath
srcCssFile = "style.css"

-- | Name of highlight script in output and resource directory.
highlightScript :: FilePath
highlightScript = "highlight.js"

-- | Path to default CSS file.
defaultCssFile :: FilePath -> FilePath
defaultCssFile libdir = libdir </> "html" </> "solarized.css"
