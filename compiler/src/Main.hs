{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import           Data.Aeson
import           Data.Args
import           Data.Binary
import           Data.List                       (intercalate, intersperse)
import           Data.Maybe                      (isJust)
import           Data.String                     (fromString)
import qualified Data.Text.Lazy                  as Text (pack, unpack)
import           Data.Time
import           Debug.Trace                     (trace)
import           Dhall
import           Dhall.Pretty                    (prettyExpr)
import           Hakyll
import           Hakyll.Commands
import           Hakyll.Core.Compiler
import qualified Hakyll.Core.Logger              as Logger
import           System.Directory                (createFileLink,
                                                  getCurrentDirectory,
                                                  getModificationTime)
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitWith)
import           System.FilePath                 (dropExtension, joinPath,
                                                  normalise, replaceDirectory,
                                                  splitDirectories, splitPath,
                                                  takeDirectory, takeFileName,
                                                  (</>))
import           Text.Blaze                      ((!))
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5                (preEscapedToHtml, toHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.Markdown


data Post = Post
    { ident :: String
    , ctime :: AnyTime
    , mtime :: AnyTime
    , body  :: H.Html
    }

postTemplate :: Post -> String
postTemplate Post{ident, ctime, mtime, body} = renderHtml $ H.docTypeHtml $ do
    H.head $ do
        -- TODO: <meta charset="utf-8">
        H.title $ toHtml name
    H.body $ do
        H.header $ do
            linkedPath ident
            H.br
            toHtml ("created: " :: String)
            dateTag ctime
            H.br
            toHtml ("modified: ":: String)
            dateTag mtime
        H.main body
    where
        name = drop 11 $ takeFileName ident
        dateTag :: AnyTime -> H.Html
        dateTag time =
            H.time
                ! HA.datetime (fromString $ fmtISOAnyTime time)
                $ toHtml $ fmtHumanAnyTime time

newtype SymLinkFile = SymLinkFile FilePath
    deriving (Binary, Eq, Show)

instance Writable SymLinkFile where
    write dst (Item _ (SymLinkFile src)) = do
        createFileLink src dst

symLinkCompiler :: Compiler (Item SymLinkFile)
symLinkCompiler = do
    path <- getResourceFilePath
    wd <- unsafeCompiler getCurrentDirectory
    makeItem $ SymLinkFile (normalise $ wd </> path)


linkHome :: String -> H.Html
linkHome s = H.a ! HA.href (fromString "/") $ toHtml $ takeFileName s

linkedPath :: String -> H.Html
linkedPath s = mapM_ id
    $ intersperse (toHtml ("/" :: String))
    $ (linkHome "ear7h.net"):links
    where
        paths = drop 1 $ reverse $ foldl f [""] $ drop 1 $ splitDirectories ("/" </> s)
        f (head:tail) x = (head ++ "/" ++ x):head:tail
        links = Prelude.map (\s -> H.a ! HA.href (fromString s) $ toHtml $ takeFileName s) paths

data AnyTime
    = MyDay Day
    | MyZTime ZonedTime

fmtHumanAnyTime t = case t of
    MyDay x   -> fmtHumanDay x
    MyZTime x -> fmtHumanZTime x

fmtISOAnyTime t = case t of
    MyDay x   -> fmtISODay x
    MyZTime x -> fmtISOZTime x

fmtHumanZTime :: ZonedTime -> String
fmtHumanZTime = formatTime defaultTimeLocale "%a, %d %B %Y %R %Z"

fmtHumanDay :: Day -> String
fmtHumanDay = formatTime defaultTimeLocale "%a, %d %B %Y"

fmtISOZTime :: ZonedTime -> String
fmtISOZTime = formatTime defaultTimeLocale "%a, %d %B %Y %R %Z"

fmtISODay :: Day -> String
fmtISODay = formatTime defaultTimeLocale "%a, %d %B %Y"

parseMarkdown :: String -> H.Html
parseMarkdown md = markdown Text.Markdown.def $ Text.pack md

parseHeader :: String -> Either String ([(String, String)], String)
parseHeader s =
    case lines s of
        [] -> Right ([], "")
        lines@(head:_) -> if isJust $ parseField head
                          then parseHeader' lines
                          else Right ([], s)


parseHeader' :: [String] -> Either String ([(String, String)], String)
parseHeader' [] = Right ([], "")
parseHeader' ("":rest) = Right ([], unlines rest)
parseHeader' (head:tail) =
    case parseField head of
        Nothing -> Left "invalid header"
        Just (k, v) -> do
            (header, body) <- parseHeader' tail
            return ((k, v):header, body)


parseField line =
    case splitOnce ':' line of
        (_, []) -> Nothing
        (k, v) -> Just (k, v)

splitOnce :: Eq a => a -> [a] -> ([a], [a])
splitOnce x [] = ([], [])
splitOnce x (head:tail)
    | x == head = ([], tail)
    | otherwise = let (head', tail') = splitOnce x tail
                  in (head:head', tail')

posts :: Rules ()
posts = do
    match "posts/*.md" do
        route $ setExtension "html"
        compile $ do
            ident <- fmap show getUnderlying
            (header, body) <- fmap (parseHeader . itemBody) getResourceString >>= handleErr ident
            mtime <- getMtime
            ctime <- handleErr ident $ getCtime ident header
            makeItem $ postTemplate Post{ident, ctime, mtime, body = (parseMarkdown body)}
    where
        handleErr name (Left s) = fail (name ++ " : " ++ s)
        handleErr _ (Right v)   = return v
        getMtime = do
            utcTime <- getResourceFilePath >>= unsafeCompiler . getModificationTime
            zone <- unsafeCompiler getCurrentTimeZone
            return $ MyZTime $ utcToZonedTime zone utcTime
        getCtime name header = case quickMapGet "Created" header of
            Nothing -> let name' = take 10 $ takeFileName name
                       in errCheck name' $ MyDay <$> parseISODay name'
            Just s -> errCheck s $ MyZTime <$> parseISOZTime s
            where
                errCheck orig val = maybeToEither ("invalid date " ++ orig) val

quickMapGet :: Eq a => a -> [(a, b)] -> Maybe b
quickMapGet _ [] = Nothing
quickMapGet x ((k, v):tail)
    | x == k = Just v
    | otherwise = quickMapGet x tail

maybeToEither s Nothing = Left s
maybeToEither _ (Just x) = Right x

parseISODay :: String -> Maybe Day
parseISODay = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing)

parseISOZTime :: String -> Maybe ZonedTime
parseISOZTime = parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M %Z")

config :: String -> String -> Configuration
config input output = defaultConfiguration
    { destinationDirectory = output
    , storeDirectory = ".hakyll-work"
    , tmpDirectory = ".hakyll-work/tmp"
    , providerDirectory = input
    , previewPort = 5000
    }

homeRules = do
    match "dump/" $ do
        route idRoute
        compile symLinkCompiler
    match "home/*" $ do
        route $ customRoute $ joinPath . tail . splitPath . toFilePath
        compile copyFileCompiler
    posts


data Module
    -- | the server will probably serve on sites.ear7h.net/~julio/{name}
    = HttpStatic
        -- | a unique identifier for the HttpStatic site
        { name        :: String
        -- | the path of files, relative to project root
        , sourcePath  :: String
        -- | files to ignore
        , ignorePaths :: [String]
        }
    -- | the server will probably serve on git.ear7h.net/~julio/{name}
    | HttpGit
        { name     :: String
        , repoPath :: String
        }
    deriving (Generic, Show)

-- TODO: ensure that the transformed paths are relative
moduleToAbs proot (HttpGit{name, repoPath}) = HttpGit
    { name
    , repoPath = proot </> repoPath
    }

moduleToAbs proot (HttpStatic{name, sourcePath, ignorePaths}) = HttpStatic
    { name
    , sourcePath = proot </> sourcePath
    , ignorePaths = Prelude.map (proot </>) ignorePaths
    }

instance FromDhall Module

instance ToDhall Module

instance ToJSON Module where
    toJSON x = case x of
        HttpGit{name, repoPath} -> object
            [ "name" .= name
            , "repoPath" .= repoPath
            ]
        HttpStatic{name, sourcePath, ignorePaths} -> object
            [ "name" .= name
            , "sourcePath" .= sourcePath
            , "ignorePaths" .= ignorePaths
            ]

data Project = Project
    { name    :: String
    , desc    :: String
    , tags    :: [String]
    , modules :: [Module]
    , root    :: String -- root path, the dirname of project.dhall
    }
    deriving (Generic, Show)

testProject = Project
    { name = "test"
    , desc = "a test project"
    , tags = ["test", "none"]
    , modules =
        [ HttpGit
            { name = "test.git"
            , repoPath = "."
            }
        ]
    , root = "/projects/test"
    }

instance FromDhall Project where

instance ToDhall Project where

data ProjectDhall = ProjectDhall
    { name    :: String
    , desc    :: String
    , tags    :: [String]
    , modules :: [Module]
    }
    deriving (Generic, Show)

instance FromDhall ProjectDhall


projectDhallToProject :: String -> ProjectDhall -> Project
projectDhallToProject proot ProjectDhall{name, desc, tags, modules}
    = Project
        { root = proot
        , name
        , desc
        , tags
        , modules = Prelude.map (moduleToAbs $ takeDirectory proot) modules
        }

readProjectPath :: FilePath -> IO Project
readProjectPath p =
    projectDhallToProject p <$> input auto (fromString p)

readProjects :: FilePath -> IO [Project]
readProjects p = input auto (fromString p)

projectsToDhallString :: [Project]-> String
projectsToDhallString proj = show $ prettyExpr s
    where
        s = (embed inject) proj

projectRules = do
    match "*/project.dhall" $ do
        compile symLinkCompiler

    create [fromFilePath "all-projects.dhall"] $ do
        route idRoute
        compile $ do
            items <- loadAll $ fromGlob "*/project.dhall"
            projects <- unsafeCompiler $ mapM (readProjectPath . symLinkPath . itemBody) items
            makeItem $ projectsToDhallString projects

    create [fromFilePath "http-git-conf.json"] $ do
        route idRoute
        compile $ do
            body <- loadBody "all-projects.dhall"
            projects <- unsafeCompiler $ readProjects (body :: String)
            let git = filter isHttpGit $ concat $ Prelude.map modules' projects
            makeItem $ Data.Aeson.encode $ toJSON git

    create [fromFilePath "http-static-conf.json"] $ do
        route idRoute
        compile $ do
            body <- loadBody "all-projects.dhall"
            projects <- unsafeCompiler $ readProjects (body :: String)
            let git = filter isHttpStatic $ concat $ Prelude.map modules' projects
            makeItem $ Data.Aeson.encode $ toJSON git
    where
        symLinkPath (SymLinkFile path) = path
        modules' = (modules :: Project -> [Module])

        isHttpGit (HttpGit {..}) = True
        isHttpGit _ = False

        isHttpStatic (HttpStatic {..}) = True
        isHttpStatic _ = False


    {-
    create [fromFilePath "http-static-conf.json"] $ do
        -- load "all-projects.dhall"
        -- filter
        undefined
-}
    -- etc...

flagAll = Flag
    { name = "all"
    , short = Just "A"
    , parser = PBool
    , def = Just (VBool False)
    , help = "rebuild all files from scratch"
    }

flagOutput = Flag
    { name = "output"
    , short = Just "o"
    , parser = PString
    , def = Just (VString "build")
    , help = "specify the output directory"
    }

flagInput s = Flag
    { name = "input"
    , short = Just "i"
    , parser = PString
    , def = Just (VString ".")
    , help = s
    }

rootCmd = RootCmd
    { name = "compiler"
    , subCmds =
        [ Cmd
            { name = "help"
            , flags = []
            , positional = Range 1 1
            , help = "show help for sub commands"
            , func = CmdFunc { unCmdFunc =
                \cv -> do
                    let positional' = positional :: CmdValue -> [String]
                    let name' = name :: Cmd -> String
                    let cmds = filter ((==) (head $ positional' cv) . name') (subCmds rootCmd)
                    putStrLn $ case cmds of
                        [] -> "subcommand not found " ++ (head $ positional' cv)
                        (head:_) -> cmdHelp "compiler" head
                }
            }
        , Cmd
            { name = "home"
            , flags =
                [ flagAll
                , flagOutput
                , flagInput "specify the directory with appropriate home files (probably the git root)"
                ]
            , positional = None
            , help = "builds simple pages like the index and posts"
            , func = CmdFunc { unCmdFunc =
                \cv -> do
                    all         <- getFlagBool cv "all"
                    output      <- getFlagString cv "output"
                    input       <- getFlagString cv "input"
                    let config' = config input output
                    let f       = if all then rebuild else build
                    logger      <- Logger.new Logger.Message
                    exitCode    <- f config' logger homeRules
                    exitWith exitCode
                }
            }
        , Cmd
            { name = "projects"
            , flags =
                [ flagAll
                , flagOutput
                , flagInput "specify the projects directory"
                ]
            , positional = None
            , help = "builds project files"
            , func = CmdFunc { unCmdFunc =
                \cv -> do
                    all         <- getFlagBool cv "all"
                    output      <- getFlagString cv "output"
                    input       <- getFlagString cv "input"
                    let config' = config input output
                    let f       = if all then rebuild else build
                    logger      <- Logger.new Logger.Message
                    exitCode    <- f config' logger projectRules
                    exitWith exitCode
                }
            }
        ]
    , help = "compiles a website"
    }

main :: IO ()
main = do
    args <- getArgs
    main' args

-- split apart main so it can be run from the repl with new args
main' :: [String] -> IO ()
main' = execRootCmd rootCmd

