{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

{- |

Modifications for the default Skylighting syntax highlighter for Haskell.

-}

module Skylighting.Extensions.Haskell (expandKeywordSet, keywords) where

-- containers
import qualified Data.Set as Set

-- skylighting-core
import Skylighting.Core

-- skylighting-modding
import Skylighting.Modding

-- text
import Data.Text (Text)

{- |

Modifies Skylighting's default Haskell syntax so that the list of keywords
includes ones that are introduced by GHC extensions (enumerated by 'keywords').

-}

expandKeywordSet :: Syntax -> Syntax
expandKeywordSet =
    modifySyntaxContexts $
        modifyContext "code" $
            replaceKeywordRule keywordRule

keywordRule :: Rule
keywordRule =
    Rule
        { rMatcher = keywordMatcher
        , rAttribute = KeywordTok
        , rIncludeAttribute = False
        , rDynamic = False
        , rCaseSensitive = True
        , rChildren = []
        , rLookahead = False
        , rFirstNonspace = False
        , rColumn = Nothing
        , rContextSwitch = []
        }

keywordMatcher :: Matcher
keywordMatcher = Keyword keywordAttr (makeWordSet True keywords)

keywordAttr :: KeywordAttr
keywordAttr =
    KeywordAttr
        { keywordCaseSensitive = True
        , keywordDelims = Set.fromList "\t\n !%&()*+,-./:;<=>?[\\]^{|}~"
        }

{- |

A list of keywords for Haskell, including ones introduced by GHC extensions.

-}

keywords :: [Text]
keywords =
    [ "case"
    , "class"
    , "data"
    , "deriving"
    , "do"
    , "else"
    , "forall" -- ExplicitForAll
    , "if"
    , "in"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "primitive"
    , "then"
    , "type"
    , "via" -- DerivingVia
    , "where"
    ]
