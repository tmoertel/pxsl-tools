-- | PxsltElementDefaults contains the code to read element-defaults
--   databases and apply them to PXSL statements.
--
-- CVS: $Id: PxsltElementDefaults.hs,v 1.6 2003/05/29 03:43:58 thor Exp $ 
--
-- Copyright (C) 2003 Tom Moertel <tom@moertel.com>
-- Licensed under the terms of the GNU General Public License.
-- See the LICENSE file in the project distribution for details.

module PxsltElementDefaults
    ( applyDefaults
    , readDefaults
    , showDefaults
    , mergeDefaults
    , xsltDefaults
    , emptyDefaults
    )
where

import PxsltParserTerms
import qualified Data.Map as Map
import Data.List (isPrefixOf, intersperse)
import Data.Char (isSpace)

type ElementDefaultDatabase = Map.Map String (String, [String])

applyDefaults :: ElementDefaultDatabase -> Statement -> Statement
applyDefaults database e@(Element _ nm pattrs nvattrs chldn) =
    case Map.lookup nm database of
        Nothing -> e { children = chldn' }
        Just (name', pnames) ->
            e { name       = name'
              , posnAttrs  = []
              , namedAttrs = zip pnames pattrs ++ nvattrs
              , children   = chldn'
              }
        where chldn' = map (applyDefaults database) chldn
applyDefaults _ e = e

readDefaults :: String -> ElementDefaultDatabase
readDefaults = Map.fromList . concatMap parseLine . stripComments . lines
    where
    stripComments = filter (any (not.isSpace)) . filter (not . isPrefixOf "#")
    parseLine l = map (\nm -> (nm, (canonName, pAttrs))) (words shortNames)
        where
        (shortNames, defnStr)  = break (=='=') l
        (canonName:pAttrs)  = words (tail defnStr)

showDefaults :: ElementDefaultDatabase -> String
showDefaults = unlines . map showEntry . Map.toList
    where
    showEntry (shortName, (canonName, pAttrs)) =
        pad 20 shortName ++ " = " ++ canonName ++ " " ++ join pAttrs
    pad n s = s ++ take (n - length s) (repeat ' ')
    join    = concat . intersperse " "

mergeDefaults :: ElementDefaultDatabase -> ElementDefaultDatabase
                                        -> ElementDefaultDatabase
mergeDefaults = Map.union

emptyDefaults :: ElementDefaultDatabase
emptyDefaults = Map.empty

xsltDefaults :: ElementDefaultDatabase
xsltDefaults = Map.fromList . concatMap copyForNS $
    [ ("stylesheet"      , ("xsl:stylesheet"      , []))
    , ("transform"       , ("xsl:transform"       , []))
    , ("import"          , ("xsl:import"          , nHref))
    , ("include"         , ("xsl:include"         , nHref))
    , ("strip-space"     , ("xsl:strip-space"     , nElements))
    , ("preserve-space"  , ("xsl:preserve-space"  , nElements))
    , ("output"          , ("xsl:output"          , []))
    , ("key"             , ("xsl:key"             , ["name","match","use"]))
    , ("decimal-format"  , ("xsl:decimal-format"  , []))
    , ("namespace-alias" , ("xsl:namespace-alias" , nNamespaceMap))
    , ("template"        , ("xsl:template"        , ["match", "name"]))
    , ("value-of"        , ("xsl:value-of"        , nSelect ++ nDisableEscing))
    , ("copy-of"         , ("xsl:copy-of"         , nSelect))
    , ("number"          , ("xsl:number"          , []))
    , ("apply-templates" , ("xsl:apply-templates" , nSelect ++ nMode))
    , ("apply-imports"   , ("xsl:apply-imports"   , []))
    , ("for-each"        , ("xsl:for-each"        , nSelect ++ nSpace))
    , ("sort"            , ("xsl:sort"            , nSelect))
    , ("if"              , ("xsl:if"              , nTest ++ nSpace))
    , ("choose"          , ("xsl:choose"          , nSpace))
    , ("when"            , ("xsl:when"            , nTest ++ nSpace))
    , ("otherwise"       , ("xsl:otherwise"       , nSpace))
    , ("attribute-set"   , ("xsl:attribute-set"   , nName ++ ["use-attribute-sets"]))
    , ("call-template"   , ("xsl:call-template"   , nName))
    , ("with-param"      , ("xsl:with-param"      , nName ++ nSelect))
    , ("variable"        , ("xsl:variable"        , nName ++ nSelect))
    , ("param"           , ("xsl:param"           , nName ++ nSelect))
    , ("text"            , ("xsl:text"            , nDisableEscing))
    , ("element"         , ("xsl:element"         , nName))
    , ("attribute"       , ("xsl:attribute"       , nName ++ nNamespace))
    , ("comment"         , ("xsl:comment"         , nSpace))
    , ("copy"            , ("xsl:copy"            , nSpace))
    , ("message"         , ("xsl:message"         , []))
    , ("fallback"        , ("xsl:fallback"        , nSpace))
    , ("processing-instruction"
                         , ("xsl:processing-instruction", nName ++ nSpace))
    ]
    where
    copyForNS e@(_, (nsName, pAttrs)) = [e, (nsName, (nsName, pAttrs))]
    nDisableEscing = ["disable-output-escaping"]
    nElements      = ["elements"]
    nHref          = ["href"]
    nMode          = ["mode"]
    nName          = ["name"]
    nNamespace     = ["namespace"]
    nNamespaceMap  = ["stylesheet-prefix", "result-prefix"]
    nSelect        = ["select"]
    nSpace         = ["xml:space"]
    nTest          = ["test"]
