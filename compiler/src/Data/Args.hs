{-# LANGUAGE DuplicateRecordFields, DisambiguateRecordFields #-}

module Data.Args
( FlagParser (..)
, Flag (..)
, PositionalMatch (..)
, RootCmd (..)
, rootCmdHelp
, Cmd (..)
, CmdFunc (..)
, cmdHelp
, FlagValue (..)
, CmdValue (..)
, parseRootCmd
, execRootCmd
, getFlag
, getFlagBool
, getFlagString
) where

import Control.Monad
import Data.List
import System.Exit (exitFailure)


data FlagParser
    = PBool
    | PNum
    | PString
    | PMultiple [FlagParser]
    deriving (Show)

-- TODO: return Either
runFlagParser :: FlagParser -> [String] -> (FlagValue, [String])
runFlagParser PBool                    x = (VBool True, x)
runFlagParser PNum                (x:xs) = (VNum (read x), xs)
runFlagParser PString             (x:xs) = (VString x, xs)
runFlagParser (PMultiple parsers)     xs =
    let
        xs' = zip parsers (takeWhile (not . hasPrefix '-') xs)
        f (p, x) = fst $ runFlagParser p [x]
    in (VMultiple $ map f xs', drop (length xs') xs)
    where
        hasPrefix c [] = False
        hasPrefix c (x:_) = c == x

data Flag = Flag
    { name :: String
    , short :: Maybe String
    , parser :: FlagParser -- [String] -> (FlagValue, [String])
    , def :: Maybe FlagValue
    , help :: String
    }
    deriving (Show)

flagHelp :: Flag -> (String, String)
flagHelp f = (name' ++ short', help' ++ def')
    where
        name' = "--" ++ (name (f :: Flag))
        short' = case (short (f :: Flag)) of
            Just x -> ", -" ++ x
            Nothing -> ""
        help' = (help (f :: Flag))
        def' = case (def (f :: Flag)) of
            Just x -> " (default: " ++ (showFlagValue x) ++ ")"
            Nothing -> ""

flagMatch :: String -> Flag -> Either String Bool
flagMatch arg flag =
    case removePrefix arg of
        (1, fname) -> Right $ (short flag) == (Just fname)
        (2, fname) -> Right $ (name (flag :: Flag)) == fname
        _ -> Left $ "invalid flag name " ++ arg
    where
        removePrefix x =
            let fname = dropWhile ('-' ==) x
             in ((length x) - (length fname), fname)

data PositionalMatch
    = Range Int Int
    | AtMost Int
    | AtLeast Int
    | None
    | Any
    deriving (Show)


runPositionalMatch :: PositionalMatch -> Int -> Bool
runPositionalMatch (Range a b) n = n >= a && n <= b
runPositionalMatch (AtMost b)  n = n <= b
runPositionalMatch (AtLeast a) n = n >= a
runPositionalMatch None        n = n == 0
runPositionalMatch Any         n = True

data RootCmd = RootCmd
    { name :: String
    , subCmds :: [Cmd]
    , help :: String
    }
    deriving (Show)

rootCmdHelp :: RootCmd -> String
rootCmdHelp rootCmd = unlines $
    [ help'
    , ""
    , "usage:"
    , tab ++ name' ++ " subcommand [opts] [args]"
    , ""
    , "subcommands:"
    ] ++ (map subCmdFmt  (subCmds rootCmd))
    where
        help' = help (rootCmd :: RootCmd)
        name' = name (rootCmd :: RootCmd)
        subCmds' = subCmds rootCmd
        maxCmdNameLen = foldl max 0 $
            map (length . (name :: Cmd -> String)) subCmds'
        subCmdFmt :: Cmd -> String
        subCmdFmt cmd = tab ++ name' ++ (nSpaces pad) ++ help'
            where
                name' = (name (cmd :: Cmd))
                help' = (help (cmd :: Cmd))
                pad = maxCmdNameLen + 4 - (length name')


data Cmd = Cmd
    { name :: String
    , flags :: [Flag]
    , positional :: PositionalMatch
    , help :: String
    , func :: CmdFunc
    }
    deriving (Show)

newtype CmdFunc = CmdFunc { unCmdFunc :: CmdValue -> IO () }

instance Show CmdFunc where
    show = const "CmdFunc {..}"

cmdMatch :: String -> Cmd -> Bool
cmdMatch n cmd = n == (name (cmd :: Cmd))

nSpaces :: Int -> String
nSpaces n = take n $ repeat ' '
tab = nSpaces 4

cmdHelp :: String -> Cmd -> String
cmdHelp rootName cmd = unlines $
    [ help (cmd :: Cmd)
    , ""
    , "usage:"
    , tab ++ rootName ++ " " ++ name' ++ " [opts]" ++ positional'
    , ""
    , "opts:"
    ] ++ map flagHelpsFmt flagHelps
    where
        name' = (name (cmd :: Cmd))
        positional' = case (positional (cmd :: Cmd)) of
            None -> ""
            _ -> " [args...]"
        flagHelps = map flagHelp (flags (cmd :: Cmd))
        maxFlagNameLen :: Int
        maxFlagNameLen = foldl max 0 $ map (length . fst) flagHelps
        flagHelpsFmt (name, msg) =
            (nSpaces 4) ++ name ++ (nSpaces pad) ++ msg
            where
                pad = maxFlagNameLen + 4 - (length name)

data FlagValue
    = VBool Bool
    | VNum Integer
    | VString String
    | VMultiple [FlagValue]
    deriving (Show)

showFlagValue :: FlagValue -> String
showFlagValue (VBool x)     = show x
showFlagValue (VNum x)      = show x
showFlagValue (VString x)   = x
showFlagValue (VMultiple x) = intercalate "," $ map showFlagValue x

assertBool :: MonadFail m => Maybe FlagValue -> m Bool
assertBool (Just (VBool x)) = return x
assertBool Nothing = fail "flag not provided"
assertBool _ = fail "not a boolean"

assertString :: MonadFail m => Maybe FlagValue -> m String
assertString (Just (VString x)) = return x
assertString Nothing = fail "flag not provided"
assertString _ = fail "not a string"


data CmdValue = CmdValue
    { name :: String
    , flags :: [(String, FlagValue)]
    , positional :: [String]
    }
    deriving (Show)

getFlagBool cv = assertBool . getFlag cv
getFlagString cv = assertString . getFlag cv

getFlag :: CmdValue -> String -> Maybe FlagValue
getFlag cv key = getFlag' (flags (cv :: CmdValue)) key
    where
        getFlag' [] _ = Nothing
        getFlag' ((headKey, headVal):tail) key
            | headKey == key = Just headVal
            | otherwise = getFlag' tail key

execRootCmd :: RootCmd -> [String] -> IO ()
execRootCmd root args = do
    (val, func) <- case parseRootCmd root args of
        Left s -> do
            putStrLn s
            putStrLn $ rootCmdHelp root
            exitFailure
        Right v -> return v
    (unCmdFunc func) val

parseRootCmd :: RootCmd -> [String] -> Either String (CmdValue, CmdFunc)
parseRootCmd root [] = Left "no args"
parseRootCmd root (arg:args) =
    case filter (cmdMatch arg) (subCmds root) of
        (x:_) -> parseCmd x (arg:args)
        _ -> Left $ "unknown sub command " ++ arg

-- Right is right and Left is error
parseCmd :: Cmd -> [String] -> Either String (CmdValue, CmdFunc)
parseCmd cmd args = do
    (cmdName, args) <- parseCmdName (name (cmd :: Cmd)) args
    (cmdFlagValues, args) <- parseCmdFlags (flags (cmd :: Cmd)) args
    let cmdFlagValuesDefaults = getFlagValueDefaults (flags (cmd :: Cmd)) cmdFlagValues
    cmdPos <-
        if runPositionalMatch (positional (cmd:: Cmd)) (length args)
        then Right args
        else Left $ "invalid positional args, expected " ++ (show $ positional (cmd :: Cmd))
    return (CmdValue
        { name = cmdName
        , flags = cmdFlagValues ++ cmdFlagValuesDefaults
        , positional = cmdPos
        }, func cmd)
    where
        parseCmdName :: String -> [String] -> Either String (String, [String])
        parseCmdName _ [] = Left "no args"
        parseCmdName name (x:xs)
            | name == x = Right (name, xs)
            | otherwise = Left "name doesn't match"

        isFlag [] = False
        isFlag (x:xs) = x == '-'


        parseCmdFlags ::
            [Flag] ->
            [String] ->
            Either String ([(String, FlagValue)], [String])
        parseCmdFlags flags [] = Right ([], [])
        parseCmdFlags flags (x:xs) = do
            (flagValues, pos, next) <-
                if x == "--"
                then Right ([], xs, [])
                else if isFlag x
                then
                    let
                        (name, val) = break ((==) '=') x
                        hasEq = val /= ""
                     in
                        if hasEq
                        then do
                            (flagValue, rest) <-
                                parseCmdFlags' flags name [tail val]
                            if rest /= []
                            then Left $ "unused =arg in " ++ x
                            else return ([flagValue], [], xs)
                        else fmap (\(x, y) -> ([x], [], y)) $
                            parseCmdFlags' flags name xs
                else Right ([], [x], xs)

            (flagValues', args'') <- parseCmdFlags flags next
            return (flagValues ++ flagValues', pos ++ args'')

        parseCmdFlags' ::
            [Flag] ->
            String ->
            [String] ->
            Either String ((String, FlagValue), [String])
        parseCmdFlags' flags fname args = do
            name' <- filterM (flagMatch fname) flags
            (case name' of
                (x:_) -> Right $
                    let (value, rest) = (runFlagParser $ parser x) args
                    in ((name (x :: Flag), value), rest)
                _ -> Left $ "unknown flag " ++ fname)

        getFlagValueDefaults ::
            [Flag] ->
            [(String, FlagValue)] ->
            [(String, FlagValue)]
        getFlagValueDefaults flags [] =
            [ (fname, d)
            | (fname, Just d) <- map (\f -> (name (f :: Flag), def f)) (flags)]
        getFlagValueDefaults flags ((x,_):xs) =
            getFlagValueDefaults (filter f flags) xs
            where
                f = ((x /=) . (name :: Flag -> String))


