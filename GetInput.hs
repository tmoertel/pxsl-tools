-- GetInput.hs
-- Tom Moertel <tom@moertel.com>
-- CVS $Id: GetInput.hs,v 1.1 2003/05/21 18:50:21 thor Exp $

-- | This module provides a method of getting input from files named
-- on the command line or, if no files are provided, from standard
-- input.  This mimics Perl's default input handling, which is
-- convenient.  Also, the module provides versions of the standard
-- 'interact' function that use these input-getting behaviors.

module GetInput ( getInputDefault, getInputFromArgs
                , interactDefault, interactFromArgs) where

import Control.Monad (liftM)
import System.Environment (getArgs)

-- | Reads the arguments passed on the command line and then passes
-- them to 'getInputFromArgs' for handling.

getInputDefault         :: IO String
getInputDefault         =  getArgs >>= getInputFromArgs

-- | Treats the input list as a list of files from which to read
-- input sequentially.  If the list is empty, input is read from
-- standard input.  If "-" is passed as a file, it is taken to
-- mean standard input.

getInputFromArgs        :: [String] -> IO String
getInputFromArgs []     =  getContents
getInputFromArgs xs     =  liftM concat (mapM readFromFile xs)
    where
    readFromFile "-"    =  getContents
    readFromFile file   =  readFile file

-- | Gets input via 'getInputDefault', processes it with the
-- function argument @f@, and then prints the @String@ that
-- @f@ returns.

interactDefault         :: (String -> String) -> IO ()
interactDefault f       =  getInputDefault >>= putStrLn . f

-- | Gets input via 'getInputFromArgs', processes it with the
-- function argument @f@, and then prints the @String@ that
-- @f@ returns.

interactFromArgs        :: [String] -> (String -> String) -> IO ()
interactFromArgs args f
                        =  getInputFromArgs args >>= putStrLn . f 


-- =================================================================
--
-- Copyright (C) 2002 Thomas Moertel.
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
-- 
-- The text of the GNU GPL may be found in the LICENSE file,
-- included with this software, or online at the following URL:
-- 
--     http://www.gnu.org/copyleft/gpl.html
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- Except as provided for under the terms of the GNU GPL, all rights
-- are reserved worldwide.
--
-- =================================================================
