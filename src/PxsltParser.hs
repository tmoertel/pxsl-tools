-- PxsltParser
--
-- CVS: $Id: PxsltParser.hs,v 1.11 2004/02/23 08:19:27 thor Exp $ 
--
-- Copyright (C) 2003 Tom Moertel <tom@moertel.com>
-- Licensed under the terms of the GNU General Public License.
-- See the LICENSE file in the project distribution for details.

module PxsltParser (pxslParser) where

import Text.ParserCombinators.Parsec
import PxsltParserTerms
import XmlString

pxslParser :: Parser [Statement]
pxslParser = do
    sts <- many (statement (-1) <?> "top-level statement")
    eof
    return sts

statement :: Int -> Parser Statement
statement col = (try empty <|> nonEmpty) <?> "statement"
    where
    nonEmpty = do
        hspaces
        indent <- getSourceColumn
        if indent <= col
           then pzero
           else ((pxslComment >> statement col)
                 <|> xmlComment <|> literal <|> element
                 <|> try macroRef <|> try macroDef <|> macroApp)

empty :: Parser Statement
empty = blankLine >> return Empty

pxslComment :: Parser ()
pxslComment = do 
    char '#'
    manyTill anyChar (skip newline <|> eof)
    return ()

xmlComment :: Parser Statement
xmlComment = do
    ctx <- getSourceContext
    string "--" <?> "comment delimiter"
    text <- manyTill anyChar (skip newline <|> eof)
    return (Comment ctx text)

literal :: Parser Statement
literal = do
    ctx <- getSourceContext
    xstr <- quotedXmlString
    hspaces
    optional newline
    return (Literal ctx xstr)

element :: Parser Statement
element = do
    ctx@(_, col) <- getSourceContext
    name <- xmlName <?> "element name"
    hspaces
    posnArgs <- exprList <?> "positional arguments"
    nvpArgs <- nameValuePairList <?> "named arguments"
    children <- subStatements col
    hspaces
    return (Element ctx name posnArgs nvpArgs children)
  where
    exprList = many $ try (do optional lineContinuation
                              e <- argExpr; hspaces; return e)
    subStatements col = many (try (statement col <?> "children statements"))

macroRef :: Parser Statement
macroRef = do
    ctx@(_, col) <- getSourceContext
    string ",,"                         <?> ",, introducing macro reference"
    name <- xmlName                     <?> "macro name"
    hspaces
    posnArgs <- exprList                <?> "positional arguments"
    nvpArgs <- nameValuePairList        <?> "named arguments"
    children <- subStatements col       <?> "BODY argument"
    hspaces
    return (MacroRef ctx name posnArgs nvpArgs children)
  where
    exprList = many $ try (do optional lineContinuation
                              e <- argExpr; hspaces; return e)
    subStatements col = many (try (statement col <?> "children statements"))
    

macroDef :: Parser Statement
macroDef = do
    ctx@(_, col) <- getSourceContext
    char ','                            <?> ", introducing macro defn"
    name <- (hspace >> return "") <|> xmlName  <?> "macro name"
    parmNames <- xmlNameList            <?> "parameter list"
    char '='                            <?> "= [macro defn]"
    many1 space -- we require at least one space to disambiguate 
                -- from macro application ",test var=val"
    bodyStatements <- subStatements col <?> "macro defn body"
    hspaces
    return (MacroDef ctx name parmNames bodyStatements)
  where
    xmlNameList = many $ try (optional lineContinuation >> xmlName)
    subStatements col = many (try (statement col <?> "macro body statement"))

macroApp :: Parser Statement
macroApp = do
    ctx@(_, col) <- getSourceContext
    char ','                            <?> ", introducing macro application"
    name <- xmlName                     <?> "macro name"
    hspaces
    posnArgs <- exprList                <?> "positional arguments"
    nvpArgs <- nameValuePairList        <?> "named arguments"
    children <- subStatements col       <?> "BODY argument"
    hspaces
    return (MacroApp ctx name posnArgs nvpArgs children)
  where
    exprList = many $ try (do optional lineContinuation
                              e <- argExpr; hspaces; return e)
    subStatements col = many (try (statement col <?> "children statements"))
  
nameValuePairList :: Parser [(String, [Statement])]
nameValuePairList = many (do optional lineContinuation
                             nvp <- nameValuePair
                             optional lineContinuation <|> hspaces1
                             return nvp)
    where
    nameValuePair = do
        try (char '-' >> notFollowedBy (char '-')) <?> "-name=value pair"
        optName <- xmlName          <?> "name for -name=value pair"
        spacesDelimited (char '=')  <?> "equals sign for -name=value pair"
        optValue <- expr            <?> "value for -name=value pair"
        hspaces
        return (optName, optValue)

expr :: Parser [Statement]
expr = (exprList <|> exprSingle) <?> "expression"
    where
    exprSingle = do
        ctx <- getSourceContext
        lit <- quotedXmlString <|> rawString
        return [Literal ctx lit]
    exprList = do
        pxp <- parenExpr <|> evalExpr
        pxps <- option [] expr
        return (pxp ++ pxps)
    rawString = do
        str <- many1 rawChar
        return (XSCdata str)
    rawChar :: Parser Char
    rawChar = do
        notFollowedBy space
        try ((try (string ")>") >> unexpected ")>")
             <|> return ())
        anyChar

evalExpr :: Parser [Statement]
evalExpr = do
    ctx <- getSourceContext
    try (string "<(") <?> "opening <( for eval exprssion"
    spaces -- layout starts at next non-whitespace char
    sts <- many (statement (-1)) <?> "eval-expression statements"
    spaces
    string ")>" <?> "closing )> for eval expression"
    return [Eval ctx sts]

parenExpr :: Parser [Statement]
parenExpr = pexp <?> "parenthesized expression"
    where
    pexp = do
        ctx <- getSourceContext
        middle <- inBetweenParens
        return [Literal ctx . XSMixed $ middle]
    inBetweenParens = do
        gexp <- between (char '(') (char ')') generalExpr
        return $ "(" ++ gexp ++ ")"
    generalExpr = do
        pieces <- many (inBetweenParens <|> many1 (noneOf "()"))
        return (concat pieces)

argExpr :: Parser [Statement]
argExpr = do
    ctx <- getSourceContext
    e <- ((notFollowedBy (oneOf "-<)" <|> newline) >> expr) <|> evalExpr)
         <?> "argument expression"
    return e

xmlName :: Parser String
xmlName = do
    firstChar <- letter <|> oneOf "_:"
    rest      <- many (letter <|> digit <|> oneOf ".-_:")
    hspaces
    return (firstChar : rest)

quotedXmlString :: Parser XmlString
quotedXmlString = do
    char '<'
    cdataLiteral <|> mixedLiteral
  where
    cdataLiteral = do
        char '{'
        txt <- manyTill anyChar (string "}>")
        return (XSCdata txt)
    mixedLiteral  = do 
        char '<'
        txt <- manyTill anyChar (try (string ">>" >> notFollowedBy (char '>')))
        return (XSMixed txt)

hspaces :: Parser ()
hspaces = skipMany hspace

hspaces1 :: Parser ()
hspaces1 = skipMany1 hspace

hspace :: Parser Char
hspace = char ' '

blankLine :: Parser ()
blankLine = skipMany hspace >> newline >> return ()

lineContinuation :: Parser ()
lineContinuation = (char '\\' >> newline >> hspaces) <?> "line continuation"

getSourceColumn :: Parser Int
getSourceColumn = getPosition >>= return . sourceColumn

getSourceLine :: Parser Int
getSourceLine = getPosition >>= return . sourceLine

getSourceContext :: Parser SourceContext
getSourceContext = do
    line <- getSourceLine
    col <- getSourceColumn
    return (line, col)

skip :: Parser a -> Parser ()
skip a = a >> return ()

-- | Allows any amount of whitespace (including none) on either side
-- of parser 'p'.

spacesDelimited :: Parser a -> Parser a
spacesDelimited p = do spaces; val <- p; spaces; return val

