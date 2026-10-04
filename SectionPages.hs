{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns, NamedFieldPuns #-}

module SectionPages
	( writeSectionFiles
	, writeSingleSectionFile
	, writeFiguresFile
	, writeFigureFiles
	, writeTablesFile
	, writeTableFiles
	, writeIndexFiles
	, writeFootnotesFile
	, writeXrefDeltaFiles
	) where

import Prelude hiding ((++), (.), writeFile)
import Control.Monad (forM_)
import Control.Arrow (first)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as TextBuilder
import LaTeXBase (LaTeXUnit(..), ArgKind(..))
import Pages (writePage, pageContent, PageStyle(..), fileContent, Link(..))
import Render (render, concatRender, renderFig, abbrHref,
	defaultRenderContext, renderTab, RenderContext(..), Page(..),linkToSection, squareAbbr,
	secnum, renderLatexParas, isSectionPage, parentLink, renderIndex)
import Document
import Util (urlChars, (++), (.), h, anchor, xml, Anchor(..), Text, intercalateBuilders)

renderParagraph :: RenderContext -> TextBuilder.Builder
renderParagraph ctx@RenderContext{nearestEnclosing=Left Paragraph{..}, draft=Draft{..}} =
		(case paraNumber of
			Just i -> renderNumbered (Text.pack $ show i)
			Nothing -> id)
		$ (if paraInItemdescr then xml "div" [("class", "itemdescr")] else id)
		$ (sourceLink
		  ++ renderLatexParas paraElems ctx'{extraIndentation=if paraInItemdescr then 12 else 0})
			-- the 12 here must match div.itemdescr's margin-left value in mm
	where
		urlBase = Text.replace "/commit/" "/tree/" commitUrl ++ "/source/"
		sourceLink :: TextBuilder.Builder
		sourceLink
			| Just SourceLocation{..} <- paraSourceLoc =
				xml "div" [("class", "sourceLinkParent")]
				$ render anchor
					{ aClass = "sourceLink"
					, aText = "#"
					, aHref = urlBase ++ Text.pack (sourceFile ++ "#L" ++ show sourceLine) } ctx
			| otherwise = ""

		renderNumbered :: Text -> TextBuilder.Builder -> TextBuilder.Builder
		renderNumbered n =
			let
				idTag = if isSectionPage (page ctx) then [("id", mconcat (idPrefixes ctx) ++ n)] else []
				a = anchor
					{ aClass = "marginalized"
					, aHref  =
						if isSectionPage (page ctx)
							then "#" ++ urlChars (mconcat (idPrefixes ctx)) ++ n
							else "SectionToSection/" ++ urlChars (abbreviation paraSection) ++ "#" ++ n
					, aText  = TextBuilder.fromText n }
				classes = "para" ++
					(if all (not . normative) (paraElems >>= sentences >>= sentenceElems)
						then " nonNormativeOnly"
						else "")
			in
				xml "div" (("class", classes) : idTag) .
				(xml "div" [("class", "marginalizedparent")] (render a ctx') ++)
		ctx' = case paraNumber of
			Just n -> ctx{ idPrefixes = idPrefixes ctx ++ [Text.pack (show n) ++ "."] }
			Nothing -> ctx
renderParagraph _ = undefined

tocSection :: RenderContext -> Section -> TextBuilder.Builder
tocSection ctx s@Section{..}
    | showSectionKindInToc sectionKind = header ++ mconcat (tocSection ctx . subsections)
    | otherwise = ""
  where
    header = h (min 4 $ 1 + length parents) $
        secnum 0 "" s ++ " "
        ++ render ( sectionName ++ [TeXRaw " "]
                  , anchor{ aHref = abbrHref abbreviation ctx, aText = squareAbbr True abbreviation, aClass="abbr_ref" })
                  ctx{ inSectionTitle = True }
        ++ "<div style='clear:right'></div>"

renderSection :: RenderContext -> Maybe Section -> Bool -> Section -> (TextBuilder.Builder, Bool)
renderSection context specific parasEmitted s@Section{abbreviation, subsections, sectionFootnotes, paragraphs}
	| full = (, True) $
		idDiv header ++
		(if specific == Just s && any (showSectionKindInToc . sectionKind) subsections then toc else "") ++
		mconcat (map
			(\p -> renderParagraph (context{nearestEnclosing=Left p,idPrefixes=if parasEmitted then [secOnPage ++ "-"] else []}))
			paragraphs) ++
		(if null sectionFootnotes then "" else "<div class='footnoteSeparator'></div>") ++
		concatRender sectionFootnotes context{nearestEnclosing=Right s} ++
		mconcat (fst . renderSection context Nothing True . subsections)
	| not anysubcontent = ("", False)
	| otherwise =
		( header ++
		  mconcat (fst . renderSection context specific False . subsections)
		, anysubcontent )
	where
		idDiv
			| specific == Just s = id
			| otherwise = xml "div" [("id", secOnPage), ("class", "section")]
		secOnPage :: Text
		secOnPage = case page context of
			SectionPage parent -> parentLink parent abbreviation
			_ -> abbreviation
		full = specific == Nothing || specific == Just s
		reduceHeaderIndent = case page context of
		    SectionPage p | specific == Nothing -> length (parents p) + 1
		    _ -> 0
		header = sectionHeader reduceHeaderIndent (min 4 $ 1 + length (parents s)) s
			(if specific == Nothing && isSectionPage (page context) then "#" ++ urlChars secOnPage else "")
			abbr context
		toc = "<hr>" ++ mconcat (tocSection context . subsections) ++ "<hr>"
		abbr
			| specific == Just s && not (null (parents s))
				= anchor
			| Just sp <- specific, sp /= s, not (null (parents s))
				= anchor{aHref = "SectionToSection/" ++ urlChars abbreviation ++ "#" ++ parentLink s (Document.abbreviation sp)}
			| otherwise = linkToSection
					(if null (parents s) then SectionToToc else SectionToSection)
					abbreviation
		anysubcontent =
			or $ map (snd . renderSection context specific True)
			   $ subsections

sectionFileContent :: PageStyle -> TextBuilder.Builder -> TextBuilder.Builder -> Text
sectionFileContent sfs title body = pageContent sfs $ fileContent pathHome title sectionPageCss body
  where
    pathHome = if sfs == InSubdir then "../" else ""
    sectionPageCss =
        "<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "expanded.css' title='Normal'>" ++
        "<link rel='alternate stylesheet' type='text/css' href='" ++ pathHome ++ "colored.css' title='Notes and examples colored'>" ++
        "<link rel='alternate stylesheet' type='text/css' href='" ++ pathHome ++ "normative-only.css' title='Notes and examples hidden'>"

writeSectionFile :: FilePath -> FilePath -> PageStyle -> TextBuilder.Builder -> TextBuilder.Builder -> IO ()
writeSectionFile out n sfs title body = writePage out n sfs (sectionFileContent sfs title body)

sectionHeader :: Int -> Int -> Section -> Text -> Anchor -> RenderContext -> TextBuilder.Builder
sectionHeader reduceIndent hLevel s@Section{..} secnumHref abbr_ref ctx
    | DefinitionSection _ <- sectionKind =
        xml "h4" [("style", "margin-bottom:3pt")] $ num ++ abbrR ++ name
    | sectionKind == UnnumberedChapter = h hLevel name
    | null parents, AnnexSection norm <- sectionKind =
        xml "div" [("class", "annexnum")] (h hLevel $ TextBuilder.fromString $ "Annex " ++ [['A'..] !! sectionNumber]) ++
        xml "div" [("class", "annexnormativity")] (if norm then "(normative)" else "(informative)") ++
        h hLevel (name ++ " " ++ abbrR)
    | BehaviorSection _ cat ab <- sectionKind =
        h hLevel $ num ++ abbrR ++ xml "div" [("class", "behaviorspecifiedin")] ("Specified in: " ++
            render (TeXComm "ref" "" [(FixArg, [TeXRaw $ cat ++ "x:" ++ ab])]) ctx)
    | otherwise = h hLevel $ num ++ " " ++ name ++ " " ++ abbrR
  where
    num = secnum reduceIndent secnumHref s
    abbrR = render abbr_ref{aClass = "abbr_ref", aText = squareAbbr False abbreviation} ctx
    name = render sectionName ctx{inSectionTitle=True}

writeFiguresFile :: FilePath -> PageStyle -> Draft -> IO ()
writeFiguresFile out sfs draft = writeSectionFile out "fig" sfs "14882: Figures" $
	"<h1>Figures <a href='SectionToToc/fig' class='abbr_ref'>[fig]</a></h1>"
	++ mconcat (uncurry r . figures draft)
	where
		r :: Paragraph -> Figure -> TextBuilder.Builder
		r p f@Figure{..} =
			renderFig True f ("./SectionToSection/" ++ urlChars figureAbbr) False True ctx
			where ctx = defaultRenderContext{draft=draft, nearestEnclosing=Left p, page=FiguresPage}

writeTablesFile :: FilePath -> PageStyle -> Draft -> IO ()
writeTablesFile out sfs draft = writeSectionFile out "tab" sfs "14882: Tables" $
	"<h1>Tables <a href='SectionToToc/tab' class='abbr_ref'>[tab]</a></h1>"
	++ mconcat (uncurry r . tables draft)
	where
		r :: Paragraph -> Table -> TextBuilder.Builder
		r p t@Table{tableSection=Section{..}, ..} =
			renderTab True t ("./SectionToSection/" ++ urlChars tableAbbr) False True ctx
			where ctx = defaultRenderContext{
				draft = draft,
				nearestEnclosing = Left p,
				page = TablesPage,
				idPrefixes = [fromJust (Text.stripPrefix "tab:" tableAbbr) ++ "-"]}

writeFootnotesFile :: FilePath -> PageStyle -> Draft -> IO ()
writeFootnotesFile out sfs draft = writeSectionFile out "footnotes" sfs "14882: Footnotes" $
	"<h1>List of Footnotes</h1>"
	++ mconcat (uncurry r . footnotes draft)
	where
		r :: Section -> Footnote -> TextBuilder.Builder
		r s fn = render fn defaultRenderContext{draft=draft, nearestEnclosing = Right s, page=FootnotesPage}

writeSingleSectionFile :: FilePath -> PageStyle -> Draft -> String -> IO ()
writeSingleSectionFile out sfs draft abbr = do
	let
	  Just section@Section{..} = Document.sectionByAbbr draft (Text.pack abbr)
	  baseFilename = Text.unpack abbreviation
	  ctx = defaultRenderContext{ draft = draft, page = SectionPage section}
	  title
	    | sectionKind == UnnumberedChapter = render sectionName ctx
	    | otherwise = squareAbbr False abbreviation
	writeSectionFile out baseFilename sfs title $ mconcat $
	    fst . renderSection ctx (Just section) False . chapters draft
	putStrLn $ "  " ++ baseFilename

writeTableFiles :: FilePath -> PageStyle -> Draft -> IO ()
writeTableFiles out sfs draft =
	forM_ (snd . tables draft) $ \tab@Table{..} -> do
		let
			context = defaultRenderContext{draft=draft, page=TablePage tab, nearestEnclosing=Right tableSection}
			header :: Section -> TextBuilder.Builder
			header sec = sectionHeader 0 (min 4 $ 1 + length (parents sec)) sec "" anchor{aHref=href} context
				where href="SectionToSection/" ++ urlChars (abbreviation sec) ++ "#" ++ urlChars tableAbbr
			headers = mconcat $ map header $ reverse $ tableSection : parents tableSection
		writeSectionFile out (Text.unpack tableAbbr) sfs (TextBuilder.fromText $ "[" ++ tableAbbr ++ "]") $
			headers ++ renderTab True tab "" True False context

writeFigureFiles :: FilePath -> PageStyle -> Draft -> IO ()
writeFigureFiles out sfs draft =
	forM_ (snd . figures draft) $ \fig@Figure{..} -> do
		let
			context = defaultRenderContext{draft=draft, page=FigurePage fig, nearestEnclosing=Right figureSection}
			header :: Section -> TextBuilder.Builder
			header sec = sectionHeader 0 (min 4 $ 1 + length (parents sec)) sec "" anchor{aHref=href} context
				where href="SectionToSection/" ++ urlChars (abbreviation sec) ++ "#" ++ urlChars figureAbbr
			headers = mconcat $ map header $ reverse $ figureSection : parents figureSection
		writeSectionFile out (Text.unpack figureAbbr) sfs (TextBuilder.fromText $ "[" ++ figureAbbr ++ "]") $
			headers ++ renderFig True fig "" True False context

writeSectionFiles :: FilePath -> PageStyle -> Draft -> [IO ()]
writeSectionFiles out sfs draft = flip map (zip names contents) $ \(n, content) ->
		writePage out n sfs content
	where
		secs = Document.sections draft
		renSec section@Section{..} = (Text.unpack abbreviation, sectionFileContent sfs title body)
		  where
			title
			    | sectionKind == UnnumberedChapter = render sectionName defaultRenderContext{draft=draft}
			    | otherwise = squareAbbr False abbreviation
			body = mconcat $ fst . renderSection (defaultRenderContext{draft=draft,page=SectionPage section}) (Just section) False . chapters draft
		fullbody = mconcat $ fst . renderSection defaultRenderContext{draft=draft, page=FullPage} Nothing True . chapters draft
		fullfile = ("full", sectionFileContent sfs "14882" fullbody)
		files = fullfile : map renSec secs
		names = fst . files
		contents = snd . files

writeIndexFile :: FilePath -> PageStyle -> Draft -> String -> IndexTree -> IO ()
writeIndexFile out sfs draft cat index =
	writeSectionFile out cat sfs ("14882: " ++ indexCatName cat) $
		h 1 (indexCatName cat) ++ renderIndex defaultRenderContext{page=IndexPage (Text.pack cat), draft=draft} index

writeIndexFiles :: FilePath -> PageStyle -> Draft -> Index -> [IO ()]
writeIndexFiles out sfs draft index = flip map (Map.toList index) $ uncurry (writeIndexFile out sfs draft) . first Text.unpack

-- Deduplicated: xrefdelta.tex can list an entry twice, and two parallel writes to one file fail.
writeXrefDeltaFiles :: FilePath -> PageStyle -> Draft -> [IO ()]
writeXrefDeltaFiles out sfs draft = flip map (Map.toList $ Map.fromList $ xrefDelta draft) $ \(from, to) ->
	writeSectionFile out (Text.unpack from) sfs (squareAbbr False from) $
		if to == []
			then "Subclause " ++ squareAbbr False from ++ " was removed."
			else "See " ++ intercalateBuilders ", " (flip render ctx . to) ++ "."
	where ctx = defaultRenderContext{draft=draft, page=XrefDeltaPage}
