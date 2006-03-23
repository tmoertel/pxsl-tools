-- | pxslcc - compiles PXSL into XML
--
-- CVS: $Id: pxslcc.hs,v 1.11 2005/01/27 14:43:52 thor Exp $ 
--
-- Copyright (C) 2003-05 Tom Moertel <tom@moertel.com>
-- Licensed under the terms of the GNU General Public License.
-- See the LICENSE file in the project distribution for details.

module Main (main) where

import Control.Monad (liftM)
import Data.Char (ord, chr)
import Data.Maybe (maybe)
import System.IO 
import System.Environment (getArgs)
import System.Console.GetOpt
import Text.ParserCombinators.Parsec

import GetInput
import PxsltElementDefaults
import PxsltFormat
import PxsltMacros
import PxsltParser
import PxsltParserTerms
import PxsltParserTreeTransforms
import UTF8 (fromUTF, toUTF)

printVersion :: IO ()
printVersion =
    putStrLn "pxslcc @VERSION@ Copyright 2003-@YEAR@ Thomas Moertel"

main :: IO ()
main = do
       rawArgs <- getArgs
       case getOpt Permute opts rawArgs of
           (flags, args, []) -> run args flags
           (_, _, errs)      -> handleArgErrors errs

data Flag = Dump | UseDB String | UseXsltDB | ExportDB | Indent Indenter
          | AddHeader | Version

opts :: [OptDescr Flag]
opts =
    [ o "i" ["indent"] (OptArg mkIndent "NUM")  
                      "Re-indent XML using NUM spaces per nesting level"
    , o "h" ["header"] (NoArg AddHeader)
                  "Insert edit-the-PXSL-instead header into output XML"
    , o "x" ["xslt"] (NoArg UseXsltDB)     "Add XSLT defaults"
    , o "a" ["add"]  (ReqArg UseDB "FILE") "Add the given defaults file"
    , o ""  ["export"] (NoArg ExportDB)
                            "Export (print) all of the active defaults"
    , o ""  ["dump"] (NoArg Dump) "Dump internal parse format (for debugging)"
    , o "v" ["version"] (NoArg Version) "Print version number and exit"
    
    ]
    where
    o = Option
    mkIndent = Indent . nestingIndenter . maybe 2 read 

run :: [String] -> [Flag] -> IO ()
run args flags = do
    case [() | Version <- flags] of
      _:_ -> printVersion
      _   -> run2
  where
  run2 = do
    loadedDBs <- mapM loadDB [fileName | UseDB fileName <- flags]
    let indenter  = head [i  | Indent i <- flags++[Indent sourceIndenter]]
    let formatter = case [() | Dump <- flags] of
                    [] -> formatWithIndenter indenter . tightenTree
                    _  -> (++"\n").show
    let combinedDB = mkCombinedDB loadedDBs
    let addHead   = case [() | AddHeader <- flags] of
                    [] -> id
                    _  -> addHeader
    case [() | ExportDB <- flags] of
      [] ->  parseAndPrint addHead (formatter . applyDefaults combinedDB) args
      _  ->  putStr (   "# BEGIN PXSL ELEMENT DEFAULTS DATABASE\n" 
                     ++ showDefaults combinedDB
                     ++ "# END PXSL ELEMENT DEFAULTS DATABASE\n")
    where
    loadDB fn = readFile fn >>= return . readDefaults
    builtinDBs = [xsltDefaults | UseXsltDB <- flags]
    mkCombinedDB dbs = foldl mergeDefaults emptyDefaults (builtinDBs ++ dbs)
    tightenTree = liftTrailingEmpties . cullLiteralDominatedEmpties

handleArgErrors :: [String] -> IO ()
handleArgErrors errs =
    ioError (userError (concat errs ++ usageInfo header opts))
    where
    header = "Usage: pxslcc [OPTION...] [file]"

parseAndPrint :: (String->String) -> (Statement -> String) -> [String] -> IO ()
parseAndPrint headerFn formatFn args = do
    input <- liftM fromUTF8 (getInputFromArgs args)
    case parse pxslParser (fileName args) input of
        Left err -> do hPutStrLn stderr (show err); ioError (userError "Parse error; aborted.")
        Right x  -> putStr . toUTF8 . headerFn $
                    concatMap formatFn (applyMacros emptyEnv x)
    where
    fileName args = case args of f:_ -> f; _ -> ""
    toUTF8 s = (map (chr. fromIntegral) $ toUTF s)
    fromUTF8 s = fromUTF (map (fromIntegral . ord) s)

addHeader :: String -> String
addHeader = unlines . addHeader'  . lines
    where
    addHeader' (pi@('<':'?':_):rest) = pi : header : rest
    addHeader' doc                   = header : doc
    header =    "<!--\n\n"
             ++ "NOTICE:  This XML document was generated from PXSL source.\n"
             ++ "         If you want to edit this file, you should probably\n"
             ++ "         edit the original PXSL source file instead.\n\n"
             ++ "-->\n"
