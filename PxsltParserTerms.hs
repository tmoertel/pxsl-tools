-- | Base PXSL data types.
-- 
-- CVS: $Id: PxsltParserTerms.hs,v 1.1 2005/01/27 14:43:52 thor Exp $ 
--
-- Copyright (C) 2003 Tom Moertel <tom@moertel.com>
-- Licensed under the terms of the GNU General Public License.
-- See the LICENSE file in the project distribution for details.

module PxsltParserTerms where

import XmlString

type Line = Int
type Column = Int
type SourceContext = (Line, Column)
type Attribute = ( String, [Statement] )

data Statement = Element { context     :: SourceContext
                         , name        :: String
                         , posnAttrs   :: [[Statement]]
                         , namedAttrs  :: [Attribute]
                         , children    :: [Statement] }
               | Literal { context     :: SourceContext
                         , xstr        :: XmlString }
               | Comment { context     :: SourceContext
                         , text        :: String }
               | MacroDef{ context     :: SourceContext
                         , name        :: String
                         , parms       :: [String]
                         , body        :: [Statement] }
               | MacroApp{ context     :: SourceContext
                         , name        :: String
                         , posnArgs    :: [[Statement]]
                         , namedArgs   :: [(String, [Statement])]
                         , children    :: [Statement] }
               | MacroRef{ context     :: SourceContext
                         , name        :: String
                         , posnArgs    :: [[Statement]]
                         , namedArgs   :: [(String, [Statement])]
                         , children    :: [Statement] }
               | Eval    { context     :: SourceContext
                         , children    :: [Statement] }
               | Error   { context     :: SourceContext
                         , text        :: String }
               | Empty  -- ^ blank line

               deriving (Eq, Read)


instance Show Statement where
    showsPrec _ l@Literal{} = ("XML: " ++) . xsToXmlS (xstr l)
    showsPrec _ c@Comment{} = ("Comment: " ++) . (text c ++)
    showsPrec _ Empty       = ("Empty" ++)
    showsPrec p e@Element{} = ("Element: " ++) . (name e ++) . (' ':) . attrs
                            . foldr (.) id (map show1 (children e))
        where
        attrs = showsPrec p (posnAttrs e) . (' ':) . showsPrec p (namedAttrs e)
        show1 child = (concatMap ("\n  "++) (lines (show child)) ++)
    showsPrec p md@MacroDef{} = ("MacroDef: " ++) . (name md ++) . (' ':)
                              . showsPrec p (parms md) . (" =" ++)
                              . foldr (.) id (map show1 (body md))
        where
        show1 child = (concatMap ("\n  "++) (lines (show child)) ++)
    showsPrec p e@MacroApp{} = ("MacroApp: " ++) . (name e ++) . (' ':) . attrs
                            . foldr (.) id (map show1 (children e))
        where
        attrs = showsPrec p (posnArgs e) . (' ':) . showsPrec p (namedArgs e)
        show1 child = (concatMap ("\n  "++) (lines (show child)) ++)
    showsPrec p e@MacroRef{} = ("MacroRef: " ++) . (name e ++) . (' ':) . attrs
                            . foldr (.) id (map show1 (children e))
        where
        attrs = showsPrec p (posnArgs e) . (' ':) . showsPrec p (namedArgs e)
        show1 child = (concatMap ("\n  "++) (lines (show child)) ++)
    showsPrec p e@Eval{} = ("Eval: " ++)
                            . foldr (.) id (map show1 (children e))
        where
        show1 child = (concatMap ("\n  "++) (lines (show child)) ++)
    showsPrec p (Error ctx text) = ("Error: "++) . showsPrec p ctx
                                 . (": "++) . (text++)
