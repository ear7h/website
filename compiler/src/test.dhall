let
    p = ./project.dhall
let
    Module = p.Module
let
    Project = p.Project
in
    { name = "test-proj"
    , desc = "a project for tests"
    , tags = [] : List Text
    , modules =
        [ Module.HttpGit
            { name = "test-proj"
            , repoPath = "."
            }
        , Module.HttpStatic
            { name = "test-proj"
            , sourcePath = "docs"
            , ignorePaths = [] : List Text
            }
        ]
    } : Project
