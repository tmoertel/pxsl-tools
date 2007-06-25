-- | Strings representing XML content.
-- 
-- CVS: $Id: XmlString.hs,v 1.4 2005/01/27 14:43:52 thor Exp $ 
--
-- Copyright (C) 2003 Tom Moertel <tom@moertel.com>
-- Licensed under the terms of the GNU General Public License.
-- See the LICENSE file in the project distribution for details.

module XmlString where

data XmlString = XSMixed String
               | XSCdata String
               | XSRun [XmlString]
               deriving (Eq, Read, Show)

xsToXml :: XmlString -> String
xsToXml = flip xsToXmlS ""

xsToXmlS :: XmlString -> String -> String
xsToXmlS (XSCdata s)  = (escapeEntities s ++)
xsToXmlS (XSMixed s)  = (s ++)
xsToXmlS (XSRun ss)   = foldr (.) id (map xsToXmlS ss)

xsToString :: XmlString -> String
xsToString = flip xsToStringS ""

xsToStringS :: XmlString -> String -> String
xsToStringS (XSMixed s) = (s ++)
xsToStringS (XSCdata s) = (s ++)
xsToStringS (XSRun ss)  = foldr (.) id (map xsToStringS ss)

xsToAttrVal :: XmlString -> String
xsToAttrVal = xsToXml

{-  no such thing as mixed attr values so we treat all as cdata

xsToAttrValS :: XmlString -> String -> String
xsToAttrValS (XSCdata s)  = (escapeEntities s ++)
xsToAttrValS (XSMixed s)  = (escapeEntities s ++)
xsToAttrValS (XSRun ss)   = foldr (.) id (map xsToAttrValS ss)

-}

escapeEntities :: String -> String
escapeEntities = concatMap quote
    where
    quote '&' = "&amp;"
    quote '<' = "&lt;"
    quote x   = [x]
