# compiler

The website compiler

## issues

An unorthodox combination of an old resolver with an overriden compiler has
to be used because of a combination of the following issues:

* [hakyll is not in the latest LTS resolver](https://github.com/jaspervdj/hakyll/issues/832)
* [older ghc versions don't work with macos big sur](https://github.com/jaspervdj/hakyll/issues/821)
