{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, OverloadedStrings #-}

import Document (Draft(..))
import Load14882 (load14882)
import Prelude hiding ((++), (.), writeFile, readFile)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, getCurrentDirectory, copyFile)
import System.Environment (getArgs)
import Control.Monad (forM_)
import Data.Text.IO (readFile)
import qualified Control.Monad.Parallel as ParallelMonad
import Util hiding (readFile)
import Toc (writeTocFiles)
import Pages (PageStyle(..))
import System.FilePath ((</>))
import SectionPages

data CmdLineArgs = CmdLineArgs
	{ outputDir :: FilePath
	, repo :: FilePath
	, sectionFileStyle :: PageStyle
	, sectionToWrite :: Maybe String }

readCmdLineArgs :: [String] -> CmdLineArgs
readCmdLineArgs ("-o" : dir : args) = (readCmdLineArgs args){outputDir = dir}
readCmdLineArgs args = case args of
	[repo, read -> sectionFileStyle, sec] -> CmdLineArgs{sectionToWrite=Just sec, ..}
	[repo, read -> sectionFileStyle] -> CmdLineArgs{sectionToWrite=Nothing,..}
	[repo] -> CmdLineArgs{sectionFileStyle=WithExtension,sectionToWrite=Nothing,..}
	_ -> error "usage: cxxdraft-htmlgen [-o outdir] path/to/draft [sectionfilestyle [section]]"
	where outputDir = "14882"

copyFileToDir :: FilePath -> FilePath -> IO ()
copyFileToDir d f = copyFile f (d </> f)

simpleFilesToCopy :: [FilePath]
simpleFilesToCopy = [
    "14882.css",
    "expanded.css",
    "colored.css",
    "normative-only.css",
    "icon-light.png",
    "icon-dark.png"]

main :: IO ()
main = do
	cwd <- getCurrentDirectory
	CmdLineArgs{..} <- readCmdLineArgs . getArgs

	extraMacros <- readFile "macros.tex"

	setCurrentDirectory $ repo ++ "/source"
	draft@Draft{..} <- load14882 extraMacros

	setCurrentDirectory cwd
	createDirectoryIfMissing True outputDir
	forM_ simpleFilesToCopy $ copyFileToDir outputDir
	case sectionToWrite of
		Just abbr -> writeSingleSectionFile outputDir sectionFileStyle draft abbr
		Nothing -> do
			let acts =
				map (\w -> w outputDir sectionFileStyle draft)
				[ writeTocFiles
				, writeFiguresFile
				, writeFigureFiles
				, writeFootnotesFile
				, writeTablesFile
				, writeTableFiles
				] ++
				writeXrefDeltaFiles outputDir sectionFileStyle draft ++
				writeIndexFiles outputDir sectionFileStyle draft index ++
				writeSectionFiles outputDir sectionFileStyle draft

			((), took) <- measure $ ParallelMonad.sequence_ acts
			putStrLn $ "Wrote files to " ++ outputDir ++ " in " ++ show (took * 1000) ++ "ms."
