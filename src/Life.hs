{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Life where

import           Data.Distributive              ( Distributive(..) )
import qualified Data.Vector                   as V
import           Data.Functor.Rep               ( Representable(..)
                                                , distributeRep
                                                )


-- | Fixed sized Vector of length gridSize
-- A Representable instance can only be defined on fixed sized vectors since
-- normal vectors can vary in size and thus, may be empty and not indexable.
newtype VBounded a = VBounded (V.Vector a)
    deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
    distribute = distributeRep

instance Representable VBounded where
    type Rep VBounded = Int
    index (VBounded v) i = v V.! (i `mod` gridSize)
    tabulate desc = VBounded (V.generate gridSize desc)

-- | The size of one side length in a gridSize x gridSize grid.
gridSize :: Int
gridSize = 20

