module Text.Pandoc.Highlighting.Extensions
  ( defaultSyntaxMap
  , extendedSyntaxMap
  ) where

-- data-default-class
import Data.Default.Class (def)

-- pandoc
import qualified Text.Pandoc as Pandoc

-- skylighting-extensions
import qualified Skylighting.Extensions

-- skylighting-modding
import Skylighting.Modding (SyntaxMap)

{- |

Pandoc's default syntax highlighting configuration.

-}

defaultSyntaxMap :: SyntaxMap
defaultSyntaxMap = Pandoc.writerSyntaxMap def

{- |

Pandoc's default syntax highlighting configuration, plus all extensions
provided by the @skylighting-extensions@ package. Use this for the value
of 'Pandoc.writerSyntaxMap' to use all of the extensions in your Pandoc
output.

-}

extendedSyntaxMap :: SyntaxMap
extendedSyntaxMap = Skylighting.Extensions.applyAll defaultSyntaxMap
