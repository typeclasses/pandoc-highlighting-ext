{-# LANGUAGE OverloadedStrings #-}

{- |

Provides a miscellaneous assortment of customized Skylighting syntax
highlighters.

-}

module Skylighting.Extensions (extendedSyntaxMap, applyAll) where

import qualified Skylighting.Extensions.GHCi
import qualified Skylighting.Extensions.Haskell

-- skylighting
import Skylighting (SyntaxMap)
import qualified Skylighting

-- skylighting-modding
import Skylighting.Modding

{- |

The default Skylighting syntax map, plus all of the extensions provided by
the @skylighting-extensions@ library.

-}

extendedSyntaxMap :: SyntaxMap
extendedSyntaxMap = applyAll Skylighting.defaultSyntaxMap

{- |

Apply all of the syntax extensions in the @skylighting-extensions@ library.

-}

applyAll :: SyntaxMap -> SyntaxMap
applyAll =
    addSyntax Skylighting.Extensions.GHCi.syntax .
    modifySyntax "Haskell" Skylighting.Extensions.Haskell.expandKeywordSet
