{-# OPTIONS_GHC -Wall #-}

{- |

Functions on 'WriterOptions' that modify the syntax highlighting.

-}

module Text.Pandoc.Highlighting.Extensions.WriterOptions
  ( applyAllSyntaxExtensions
  , modifySyntaxMap
  , modifySyntax
  , modifyContext
  ) where

-- pandoc
import Text.Pandoc (WriterOptions (..))

-- skylighting-core
import Skylighting.Types (Context, Syntax)

-- skylighting-extensions
import qualified Skylighting.Extensions

-- skylighting-modding
import Skylighting.Modding (SyntaxMap)
import qualified Skylighting.Modding

-- text
import Data.Text (Text)

{- |

Apply all of the syntax highlighting extensions provided by the
@skylighting-extensions@ package to Pandoc writer options.

-}

applyAllSyntaxExtensions :: WriterOptions -> WriterOptions
applyAllSyntaxExtensions = modifySyntaxMap Skylighting.Extensions.applyAll

{- |

Apply a function to the 'SyntaxMap' within Pandoc writer options.

-}

modifySyntaxMap :: (SyntaxMap -> SyntaxMap) -> WriterOptions -> WriterOptions
modifySyntaxMap f o = o{ writerSyntaxMap = f (writerSyntaxMap o) }

{- |

Modify a particular syntax by name.

-}

modifySyntax
    :: Text -- ^ Name of the syntax to modify
    -> (Syntax -> Syntax) -> WriterOptions -> WriterOptions

modifySyntax name =
    modifySyntaxMap .
    Skylighting.Modding.modifySyntax name

{- |

Modify a particular syntax context by name.

-}

modifyContext
    :: Text -- ^ Name of the syntax to modify
    -> Text -- ^ Name of the context to modify
    -> (Context -> Context) -> WriterOptions -> WriterOptions

modifyContext syntaxName contextName =
    modifySyntax syntaxName .
    Skylighting.Modding.modifySyntaxContexts .
    Skylighting.Modding.modifyContext contextName
