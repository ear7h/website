let
  Module : Type =
    < HttpStatic :
        { name : Text
        , sourcePath : Text
        , ignorePaths : List Text
        }
    | HttpGit :
        { name : Text
        , repoPath : Text
        }
    >
let
  Project : Type =
    { name : Text
    , desc : Text
    , tags : List Text
    , modules : List Module
    }
in
  { Module
  , Project
  }

