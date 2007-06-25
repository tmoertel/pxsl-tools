-- | PxsltParserTreeTransforms -- Transform parse trees
--
-- CVS: $Id: PxsltParserTreeTransforms.hs,v 1.2 2003/05/29 03:47:07 thor Exp $ 
--
-- Copyright (C) 2003 Tom Moertel <tom@moertel.com>
-- Licensed under the terms of the GNU General Public License.
-- See the LICENSE file in the project distribution for details.

module PxsltParserTreeTransforms
    ( cullLiteralDominatedEmpties
    , liftTrailingEmpties
    , trimEmpties
    , dropEmpty
    , fromBack
    , bidir
    )
where

import PxsltParserTerms


-- | Trim away Empties surrounding Literals because the literals gobble
--   surrounding whitespace.

cullLiteralDominatedEmpties :: Statement -> Statement
cullLiteralDominatedEmpties = cull
    where
    cull e@(Element _ _ _ _ children) =
        e { children = map cull (cull' children) }
    cull md@(MacroDef _ _ _ body)     =
        md { body = map cull (cull' body) }
    cull nonElement = nonElement

cull' children = children'
    where
    children' = case (dropEmpty children, dropEmpty (reverse children)) of
                (x@(Literal{}:_),   (Literal{}:_)) -> bidir dropEmpty x
                (x@(Literal{}:_),   _            ) -> x
                (  _            , y@(Literal{}:_)) -> reverse y
                (  _            ,   _            ) -> children


-- | Reparent a statement's excess trailing Empty elements so that
-- they are as high as possible in the tree.  That has the effect of
-- allowing parents to be closed early instead of late:

--   <a>                   <a>
--     <b/>                  <b/>
--               ===>      </a>
--   </a>
--   <c/>                  <c/>

liftTrailingEmpties :: Statement -> Statement
liftTrailingEmpties e@(Element _ _ _ _ cs@(_:_)) =
    e { children = concatMap (trimEmpties . liftTrailingEmpties) cs }
liftTrailingEmpties md@(MacroDef _ _ _ cs@(_:_)) =
    md { body = concatMap (trimEmpties . liftTrailingEmpties) cs }
liftTrailingEmpties x = x

trimEmpties :: Statement -> [Statement]
trimEmpties e@(Element _ _ _ _ (ch:ct)) = e { children = cs' } : trimmedEs
    where
    cs' = ch : reverse rct'
    (trimmedEs, rct') = span (==Empty) (reverse ct)
trimEmpties e@(MacroDef _ _ _ (ch:ct)) = e { children = cs' } : trimmedEs
    where
    cs' = ch : reverse rct'
    (trimmedEs, rct') = span (==Empty) (reverse ct)
trimEmpties x = [x]

-- | Drop Empty statements at the beginning of a list.

dropEmpty :: [Statement] -> [Statement]
dropEmpty = dropWhile (==Empty)

-- | Apply a list function to a list from the back.

fromBack :: ([a] -> [a]) -> [a] -> [a]
fromBack f = reverse . f . reverse

-- | Apply a list function to a list from the front and back, i.e.,
--   bidirectionally.

bidir :: ([a] -> [a]) -> [a] -> [a]
bidir f = reverse . f . reverse . f
