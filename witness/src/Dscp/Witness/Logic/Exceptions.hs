-- | Logic-related exceptions.

module Dscp.Witness.Logic.Exceptions
    ( LogicException (..)
    ) where

import Data.Data (Data)
import qualified Data.Text.Buildable as B
import qualified Text.Show

data LogicException
    = LEBlockAbsent Text
    | LETxAbsent Text
    | LEPrivateBlockAbsent Text
    | LEMalformed Text
    deriving (Eq, Data)

instance Show LogicException where
    show = toString . pretty

instance Buildable LogicException where
    build = \case
        LEBlockAbsent err -> B.build err
        LETxAbsent err -> B.build err
        LEPrivateBlockAbsent err -> B.build err
        LEMalformed err -> B.build err
