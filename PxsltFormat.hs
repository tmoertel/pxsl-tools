-- | PxsltFormat -- Format PXSLT statements for output.
--
-- CVS: $Id: PxsltFormat.hs,v 1.8 2003/06/05 19:37:02 thor Exp $ 
--
-- Copyright (C) 2003 Tom Moertel <tom@moertel.com>
-- Licensed under the terms of the GNU General Public License.
-- See the LICENSE file in the project distribution for details.

module PxsltFormat 
    ( format
    , formatWithIndenter
    , sourceIndenter
    , nestingIndenter
    , Indenter
    )
where

import Data.List (intersperse)
import Control.Monad.State

import PxsltParserTerms
import PxsltParserTreeTransforms
import XmlString

-- | The following data types

data FormatContext    = FC { shouldIndentQ :: Bool, nesting :: Int }
type Indenter         = Int -> Int -> Int
type FormatterState   = State (Indenter, FormatContext)
type FormatterStringS = FormatterState (String -> String)

sourceIndenter :: Indenter
sourceIndenter sourceColumn    _ = sourceColumn     -- use indent from source

nestingIndenter :: Int -> Indenter
nestingIndenter n _ nestingDepth = n * nestingDepth -- use nesting for indent

getIndent :: Int -> FormatterStringS
getIndent sourceColumn = do
    (indenter, ctx) <- get
    return $ if shouldIndentQ ctx
       then spacer (indenter sourceColumn (nesting ctx))
       else id

allowIndenting :: Bool -> FormatterState ()
allowIndenting allowQ =
    modify (\ (idr, ctx) -> (idr, ctx { shouldIndentQ = allowQ }))

modifyNesting :: (Int -> Int) -> FormatterState ()
modifyNesting f =
    modify (\ (idr, ctx) -> (idr, ctx { nesting = f (nesting ctx) }))

spacer :: Int -> (String -> String)
spacer n = (replicate n ' ' ++)

-- | Formats a PXSL statement as XML using the PXSL source's indenting.

format :: Statement -> String
format = formatWithIndenter const

-- | Formats a PXSL statement as XML.

formatWithIndenter :: Indenter -> Statement -> String
formatWithIndenter indenter stmt =
    evalState (formatter stmt) state0 $ ""
    where
    state0 = (indenter, FC { shouldIndentQ = True, nesting = 0 })

formatter :: Statement -> FormatterStringS
formatter stmt = case stmt of

    Empty         -> allowIndenting True  >> return ('\n':)

    Literal _ xs  -> allowIndenting False >> return (xsToXmlS xs)

    Comment (_, scol+1) txt -> do
        indent <- getIndent scol
        allowIndenting True
        return (indent . ("<!--"++) . (escComment txt ++) . (" -->\n"++))

    Element (_, scol+1) name pAttrs nAttrs children -> do
        startIndent <- getIndent scol
        allowIndenting True
        modifyNesting (+1)
        contentFmts <- mapM formatter (dropEmpty `fromBack` children)
        trailerFmts <- mapM formatter (takeWhile (==Empty) (reverse children))
        modifyNesting (flip (-) 1)
        (startTagClose, endTag) <- getIndent scol >>= return . getTags
        allowIndenting True
        let startTag = ('<':) . (name++) . pAttrRep . nAttrRep . startTagClose
        return (startIndent . startTag .   -- opening tag
                foldr (.) id contentFmts . -- element contents
                endTag .                   -- closing tag
                foldr (.) id trailerFmts)  -- trailing blank lines

        where

        getTags indentFn = 
            if all (==Empty) children
            then (("/>" ++), id)
            else ((">"++)  , indentFn . ("</" ++) . (name ++) . (">\n" ++))

        pAttrRep = case pAttrs of
                      [] -> id
                      ps -> (' ':) .
                            foldr (.) id (intersperse (' ':)
                                          (map (curry showAttr "DEFAULT") ps))
        nAttrRep = case nAttrs of
                      [] -> id
                      ns -> (' ':) .
                            foldr (.) id (intersperse (' ':) (map showAttr ns))
        
        showAttr (name, val) = (name++) . ('=':) . quote (concatMap fmtA val)
        fmtA = formatWithIndenter (nestingIndenter 2)

        quote s sn = '\"' : concatMap q s ++ "\"" ++ sn
        q '\"' =  "&#34;"
        q c    =  [c]

    Error (line, col) msg -> return $
        ("(ERROR: (line " ++) . (shows line) . (", col "++) . (shows col)
                              . (") "++) . (msg++) . (")\n"++)

    x ->
        return $ ("(UNKNOWN: " ++) . shows x . (')':)


escComment :: String -> String
escComment ""             = ""
escComment ('-':'-':rest) = "-=" ++ escComment rest
escComment (x:rest)       = x : escComment rest
