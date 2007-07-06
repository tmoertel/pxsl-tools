-- PxsltMacros
--
-- CVS: $Id: PxsltMacros.hs,v 1.9 2004/02/23 08:19:27 thor Exp $ 
--
-- Copyright (C) 2003 Tom Moertel <tom@moertel.com>
-- Licensed under the terms of the GNU General Public License.
-- See the LICENSE file in the project distribution for details.

module PxsltMacros
    ( applyMacros
    , emptyEnv
    )
where

import Data.List
import Control.Monad.Reader
import Data.Maybe (mapMaybe)

import PxsltParserTerms
import XmlString

newtype MacroEnv = MacroEnv [(String, Either (MacroEnv,Statement) [Statement])]
type MacroReader = Reader MacroEnv

applyMacros :: MacroEnv -> [Statement] -> [Statement]
applyMacros env sts = runReader (app sts) env

-- | We want to remove all MacroDefs and MacroApps from the parse
-- tree and replace the MacroApps with their results, which are
-- either Errors or expansions ([Statement]).

app :: [Statement] -> MacroReader [Statement]
app sts = do
    env <- ask
    let (envAdditions, sts') = extractMacroDefs env sts
    expansions <- local (envAdditions `joinEnv`) (mapM expandMacros sts')
    return (concat expansions)

-- | W.r.t. the given environment, extract the macro definitions in
-- the input list and return an environment that contains them (and
-- the list less the macro definitions).

extractMacroDefs :: MacroEnv -> [Statement] -> (MacroEnv, [Statement])
extractMacroDefs env sts = (mdefs, sts')
    where
    mdefs = MacroEnv . reverse $ mapMaybe mkEnvEntry sts
    env'  = mdefs `joinEnv` env -- closure for the new macros
    sts'  = filter (not . isMacroDefQ) sts
    mkEnvEntry mdef@(MacroDef _ nm _ _) = Just (nm, Left (env', mdef))
    mkEnvEntry _                        = Nothing
    isMacroDefQ MacroDef{} = True
    isMacroDefQ _          = False

-- | If the input Statement is a macro application, evaluate it in the
-- context of the active environment and return the resulting
-- statement list after the applications have been replaced with their
-- results, which can be either an Error or an expansion [Statement].
-- Then recurse through the result via app, expanding any macro
-- applications within.

expandMacros :: Statement -> MacroReader [Statement]

expandMacros (MacroApp ctx nm pArgs nArgs body) = do

    menv@(MacroEnv env) <- ask

    case lookup nm env of

        -- name doesn't exist within this evaluation environment ?
        Nothing -> return (errNotDefined nm)

        -- name exists and maps to a macro definition ?
        Just (Left (macroEnv, MacroDef mctx _ mparms mexpansion)) ->
            return $ applyMacros (enterMacro macroEnv) mexpansion
            where
            enterMacro = joinEnv . MacroEnv . map (prepareArgs menv) $
                         nArgs ++ zip mparms pArgs' ++ [("BODY",body)]
            pArgs' = pArgs ++ repeat [Literal ctx (XSCdata "(UNDEFINED)")]

        {- MacroRef support isn't official
        Just (Right (MacroRef rctx rnm rpArgs rnArgs rbody : _)) ->
        expandMacros (MacroApp  OOOOOOOOOO MaroRef work in progress OOOOO
        -}

        -- name exists and maps to a document fragment ?
        Just (Right x) -> app x

    where

    prepareArgs menv (n, [Eval _ (mdef@MacroDef{name=""}:_)]) =
        (n, Left (menv,mdef)) -- anonymous macro
    prepareArgs menv (n, x) =
        (n, Right (applyMacros menv x))
    errNotDefined nm = [Error ctx ("Could not find a value for \","
                                   ++nm++ "\".")]


expandMacros (Element ctx nm pa na chldn) = do
    pa' <- mapM app pa
    na' <- mapM (\ (n,v) -> do v' <- app v; return (n,v') ) na
    chldn' <- app chldn
    return [Element ctx nm pa' na' chldn']
expandMacros (Eval ctx sts) = do
    sts' <- app sts
    return sts'
expandMacros x = return [x]


-- | An empty environment.

emptyEnv :: MacroEnv
emptyEnv = MacroEnv []

-- | Join two environments.  The first shadows the second if
-- their are overlaps.

joinEnv :: MacroEnv -> MacroEnv -> MacroEnv
joinEnv (MacroEnv a) (MacroEnv b) = MacroEnv (a ++ b)
