{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

{- |

Introduces a new Skylighting syntax highlighter for GHCi (a REPL for Haskell).

-}

module Skylighting.Extensions.GHCi (syntax) where

-- skylighting-core
import Skylighting.Core

-- skylighting-modding
import Skylighting.Modding

-- text
import qualified Data.Text.Encoding as Text

{- |

A Skylighting 'Syntax' for highlighting GHCi sessions. The only thing this does
is highlight prompts. We assume that the prompt is @"λ>"@ and the continuation
prompt is @" >"@.

-}

syntax :: Syntax
syntax =
    Syntax
        { sName = "ghci"
        , sFilename = ""
        , sShortname = "ghci"
        , sContexts = contextMap ghciContext
        , sAuthor = "Typeclass Consulting, LLC"
        , sVersion = "1"
        , sLicense = "MIT"
        , sExtensions = []
        , sStartingContext = "ghci"
        }

ghciContext :: Context
ghciContext =
    Context
        { cName = "ghci"
        , cSyntax = "ghci"
        , cRules = [promptRule]
        , cAttribute = NormalTok
        , cLineEmptyContext = []
        , cLineEndContext = []
        , cLineBeginContext = []
        , cFallthrough = False
        , cFallthroughContext = []
        , cDynamic = False
        }

promptRule :: Rule
promptRule =
    Rule
        { rMatcher = RegExpr promptRE
        , rAttribute = AnnotationTok
        , rIncludeAttribute = True
        , rDynamic = False
        , rCaseSensitive = True
        , rChildren = []
        , rLookahead = False
        , rFirstNonspace = False
        , rColumn = Just 0
        , rContextSwitch = []
        }

promptRE :: RE
promptRE =
    RE
        { reString = Text.encodeUtf8 "[λ ]>"
        , reCaseSensitive = True
        }
