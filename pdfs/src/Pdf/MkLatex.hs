
module Pdf.MkLatex
    ( MkLatex (..)
    , the
    , custom
    , ignore
    , split
    , allThe
    , inBlock
    , shown
    , command

    , escapeInLatex

    , module Data.Functor.Contravariant
    , module Data.Functor.Contravariant.Divisible
    )
    where

import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Divisible (..), Decidable (..), divided)

import           Data.Char (isSpace)
import qualified Data.Set as Set (fromList, member)
import qualified Data.Text as Text.Strict
import           Data.Text.Lazy.Builder as Text

import Text.Printer (text)

-- | Escapes text, so it can be put inside latex code.
escapeInLatex :: Text -> Text
escapeInLatex = Text.Strict.pack . escape . Text.Strict.unpack
  where
    escape = \case
        '\\' : sp  : rest | isSpace   sp -> "\\textbackslash{ }"  ++ escape rest
        '\\' :       rest                -> "\\textbackslash{}"   ++ escape rest
        '^'  :       rest                -> "\\textasciicircum{}" ++ escape rest
        '~'  :       rest                -> "\\textasciitilde{}"  ++ escape rest
        sp   :       rest | isSpace   sp -> " "                   ++ escape rest
        ch   :       rest | isSimple  ch -> "\\" ++ [ch]          ++ escape rest
        '"'  : ru  : rest | isRussian ru -> "<<"                  ++ escape (ru : rest)
        ru   : '"' : rest | isRussian ru -> [ru] ++ ">>"          ++ escape rest
        '"'  : en  : rest | isEnglish en -> "``"                  ++ escape (en : rest)
        en   : '"' : rest | isEnglish en -> [en] ++ ['"']         ++ escape rest
        ch   :       rest                -> ch                     : escape rest
        []                               -> []
      where
        isSimple  ch = ch `Set.member` Set.fromList "{}&%$#_"
        isEnglish ch = ch `Set.member` Set.fromList (['A'.. 'Z'] ++ ['a'.. 'z'])
        isRussian ch = ch `Set.member` Set.fromList (['А'.. 'Я'] ++ ['а'.. 'я'])

-- | Generate latex from a.
data MkLatex a = MkLatex (a -> Text.Builder)

instance Contravariant MkLatex where
    contramap f (MkLatex printer) = MkLatex (printer . f)

-- | After division, the results are joined with a newline.
instance Divisible MkLatex where
    divide splitter ~(MkLatex left) ~(MkLatex right) = MkLatex $ \a -> do
        let (l, r) = splitter a
        left l <> "\n" <> right r

    conquer = MkLatex (const "")

-- | "If" procedure for `MkLatex`.
instance Decidable MkLatex where
    choose selector ~(MkLatex left) ~(MkLatex right) = MkLatex $
        either left right . selector

    lose _ = ignore

-- | Analog to `const`.
the :: Text.Builder -> MkLatex a
the txt = custom (const txt)

-- | Custom conversion to `Text.Builder`.
custom :: (a -> Text.Builder) -> MkLatex a
custom = MkLatex

-- | Produces nothing.
ignore :: MkLatex a
ignore = contramap (const ()) conquer

-- | Take part of the structure, make latex and pass the document further.
split :: (a -> b) -> MkLatex b -> MkLatex a -> MkLatex a
split proj = divide (proj &&& id)

-- | Wrap produced latex with "\\begin{name}\n ...\n\\end{name}".
inBlock :: Text.Builder -> MkLatex a -> MkLatex a
inBlock name make
    = divide (const () &&& id) (the ("\\begin{" <> name <> "}"))
    $ divide (id       &&& id) make
    $ the ("\\end{" <> name <> "}")

-- | Analoguous to `sepBy` from Parsec.
allThe :: Show a => MkLatex () -> MkLatex a -> MkLatex [a]
allThe sep maker = aux
    where
        aux = choose
            (\case
                []     -> Left ()
                x : xs -> Right (x, xs))
            ignore
            (divided
                (divide (id &&& const ()) maker sep)
                aux)

-- | For clarity.
shown :: Show x => x -> Text.Builder
shown = text . escapeInLatex . show

-- | Given command name and arg consumer, make a latex command.
command :: Text.Builder -> (a -> [Text.Builder]) -> MkLatex a
command name prepare = MkLatex $ \a ->
    "\\"
    <> name
    <> mconcat
        (map
            (\arg -> "{" <> arg <> "}")
            (prepare a))
