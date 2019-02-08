{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}

{- |

Functions for making alterations to @skylighting-core@ values ('SyntaxMap',
'Syntax', 'Context', 'Rule', etc.).

-}

module Skylighting.Modding
  (
  -- * Syntax maps
    SyntaxMap, syntaxMap, addSyntax, modifySyntax

  -- * Syntax fields
  , modifySyntaxContexts

  -- * Context maps
  , ContextMap, contextMap, addContext, modifyContext

  -- * Rules
  , replaceKeywordRule, isKeywordRule

  ) where

-- containers
import qualified Data.Map as Map
import Data.Map (Map)

-- skylighting-core
import Skylighting.Core hiding (syntaxMap)

-- text
import Data.Text (Text)

{- |

A map of contexts, keyed by 'cName'.

This is the type of 'sContexts'.

-}

type ContextMap = Map Text Context

{- |

Convert a 'Context' into a 'ContextMap' that contains only that one context.

-}

contextMap :: Context -> ContextMap
contextMap c = Map.singleton (cName c) c

{- |

Adds one 'Context' to a 'ContextMap', or replaces a context of the same name if
one exists.

-}

addContext :: Context -> ContextMap -> ContextMap
addContext c m = Map.insert (cName c) c m

{- |

Modify a 'SyntaxMap' by looking up a particular syntax by name, applying some
function to it, and placing the resulting syntax into the map in place of the
original syntax.

-}

modifyContext
    :: Text -- ^ The name of the context to modify
    -> (Context -> Context) -> ContextMap -> ContextMap

modifyContext name f m = Map.adjust f name m

{- |

Convert a 'Syntax' into a 'SyntaxMap' that contains only that one syntax.

-}

syntaxMap :: Syntax -> SyntaxMap
syntaxMap s = Map.singleton (sName s) s

{- |

Adds one 'Syntax' to a 'SyntaxMap', or replaces a context of the same name if
one exists.

-}

addSyntax :: Syntax -> SyntaxMap -> SyntaxMap
addSyntax s m = Map.insert (sName s) s m

{- |

Modify a 'SyntaxMap' by looking up a particular syntax by name, applying some
function to it, and placing the resulting syntax into the map in place of the
original syntax.

-}

modifySyntax
    :: Text -- ^ The name of the syntax to modify
    -> (Syntax -> Syntax) -> SyntaxMap -> SyntaxMap

modifySyntax name f m = Map.adjust f name m

{- |

Apply a function to the 'sContexts' field of a 'Syntax'.

-}

modifySyntaxContexts :: (ContextMap -> ContextMap) -> Syntax -> Syntax
modifySyntaxContexts f s = s { sContexts = f (sContexts s) }

{- |

Alters a 'Context' by replacing any 'Rule' that looks like a typical keyword
rule (as determined by 'isKeywordRule') with the given rule.

-}

replaceKeywordRule :: Rule -> (Context -> Context)
replaceKeywordRule newRule context =
    let
        rules =
            fmap
                (\r -> if isKeywordRule r then newRule else r)
                (cRules context)
    in
        context { cRules = rules }

{- |

Determines whether a 'Rule' looks like a typical rule for keywords:

  1. 'rAttribute' = 'KeywordTok'
  2. 'rMatcher' is of the 'Keyword' variety

-}

isKeywordRule :: Rule -> Bool
isKeywordRule =
    \case
        Rule{ rAttribute = KeywordTok, rMatcher = Keyword _ _ } -> True
        _                                                       -> False
