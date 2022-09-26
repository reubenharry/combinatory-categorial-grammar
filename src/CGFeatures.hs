{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module CGFeatures where

import Control.Applicative (Applicative (..), Alternative (empty, (<|>), some))
import Data.Text qualified as T
import Data.Functor.Foldable
    ( hylo,
      Corecursive(ana, gpostpro),
      Recursive(cata, gprepro),
      Base,
      unfold,
      futu,
      gfutu,
      distGFutu,
      gcata,
      gana,
      fold )
import Data.Functor.Compose (Compose (..))
import Control.Monad.Trans.Free (FreeT (FreeT), FreeF (Pure, Free), cutoff, free)
import Data.Void (Void, absurd)
import Text.Megaparsec
    ( empty,
      (<|>),
      runParserT,
      errorBundlePretty,
      MonadParsec(eof, try),
      ParsecT )
import Text.Megaparsec.Char (string, space)
import Data.Text (Text, pack)
import Data.Fix (Fix(..))
import qualified Data.Text.IO as T
import Control.Monad.Identity ( Identity(Identity, runIdentity) )
-- import Relude hiding (fold)
-- import Prelude ()

data Number = Plural | Singular deriving (Eq, Show)
data Features = Features { number :: Number, gap :: Gap } deriving (Eq, Show)

data Gap = Gap | NoGap deriving (Eq, Show)

data Syn = S Features | NP Features | N Features | Syn :/: Syn | Syn :\: Syn deriving Show

pattern NG <- Features {gap = NoGap}
pattern G <- Features {gap = Gap}

leaf :: Syn -> [Text]
leaf = \case
  NP Features {gap = Gap} -> [""]
  NP Features {number = Singular} ->  ["a1", "a2"]
  S Features {gap = g1, number = Singular}  :\: NP Features {gap = g2, number = Singular}
    | g1 == g2 -> ["attacks", "exists"]
  S Features {gap = Gap} :\: NP Features {number = Plural} -> ["attack", "exist"]
  (S Features {gap = g1} :\: NP s@Features {gap = g2}) :/: NP Features {gap = g3} 
    | (g1==g2 && g3==NoGap) || (g1==g3 && g2==NoGap)  -> case s of
      Features {number = Singular} -> ["attacks", "defends"]
      Features {number = Plural} -> ["attack", "defend"]
  N Features {number = Singular} -> ["bishop", "rook", "piece"]
  N Features {number = Plural} -> ["bishops", "rooks", "pieces"]
  N f1 :/: N f2 | f1==f2 -> ["black", "white"]
  (NP Features {number = Singular} :/: N Features {number = Singular}) -> ["the", "every", "a"]
  (NP Features {number = Plural} :/: N Features {number = Plural}) -> ["the"]
  S Features {gap = NoGap} :/: S Features {gap = Gap} -> ["what"]
  _ -> []

grammarRules :: Syn -> [Node Syn]
grammarRules syn1 = do
  syn2 <- syntacticCategories
  [Node (syn1 :/: syn2) syn2] <|> [Node syn2 (syn1 :\: syn2)]

idioms :: Syn -> [Idiom]
idioms = \case
  N Features {number = Singular} -> [
    branch
      (word "attacking")
      (word "move")
    ]
  ((S Features {gap = NoGap}) :\: NP sing) -> 
    [
      branch
        (word "checks")
        (branches 
          (syn (NP Features {number = Singular, gap = NoGap})) 
          (branches 
            (word "with")
            (syn (NP Features {number = Singular, gap = NoGap}))))
    ]
  NP Features {number = Plural} ->
    [branch
      (branches
        (word "all")
        (word "the"))
      (syn (N Features {number = Plural, gap = NoGap}))
    ]
  _ -> []

  where
  syn = FreeT . Identity . Pure
  branch a b = Free $ Node a b
  branches a b = FreeT $ Identity $ Free $ Compose [Free (Node a b)]
  word lemma = FreeT . Identity . Free . Compose $ [Free (Leaf lemma (S Features {number = Singular, gap = NoGap}))]

unfolder :: Unfolder Syn
unfolder x = Compose $
  (fmap . fmap) pure (Free . flip Leaf x <$> leaf x)
  <> idioms x
  <> (fmap . fmap) pure (Free <$> grammarRules x)

folder :: Folder [Text]
folder =
  foldr combine empty
  . fmap (\case
      Pure (Just a) -> absurd a
      Pure Nothing -> Node empty empty
      Free x -> x
      )
  . getCompose
  where
    combine (Node a b) c = liftA2 (\x y -> "(" <> x <> " " <> y <> ")") a b <|> c
    combine (Leaf a _) b = pure a <|> b

language :: Syn -> Tree
language = gpostpro (distGFutu (fmap Identity . runIdentity)) id unfolder

grammar :: Syn -> IO ()
grammar =
  mapM_ T.putStrLn
  . fold folder
  . cutoff 5
  . language


parser :: Folder Parser
parser ls = foldr combine empty
  $ (\case
      Pure (Just a) -> absurd a
      Pure Nothing -> Node empty empty
      Free x -> x
      ) <$> getCompose ls

  where
    combine (Node a b) c = try (do
      x <- a
      string " "
      Fix . Node x <$> b) <|> c
    combine (Leaf a syn) b = try (Fix . flip Leaf syn <$> string a ) <|> b

parse :: Syn -> Text -> IO ()
parse = flip \sentence ->
  mapM_ T.putStrLn
  . fmap (either
        (T.pack . errorBundlePretty)
        ((\(y,x) -> x <> "\n" <> y) . linearize))
  . (\x -> runParserT (x <* try eof) "" sentence)
  . fold parser
  . cutoff 5
  . language

linearize :: Fix Node -> (Text, Text)
linearize = cata \case
  Node (a,s1) (b,s2) -> (
    "(" <> a <> T.pack (replicate (20 - T.length a) ' ') <> b <> ")",
    " " <> s1 <> T.pack (replicate (20 - T.length s1) ' ') <> s2 <> " "
    )
  Leaf a s -> ("  " <> a, pack $ "  " <> display s)

display :: Syn -> String
display = \case
  NP f -> "NP{" <> displayFeatures f <> "}"
  S f -> "NP{" <> displayFeatures f <> "}"
  N f -> "NP{" <> displayFeatures f <> "}"

  a :/: b -> "(" <> display a <> "/" <> display b <> ")"
  a :\: b -> "(" <> display a <> "\\" <> display b <> ")"
  _ -> undefined

displayFeatures Features {number = n, gap = g} =
  let displayGap = \case
        Gap -> "g"
        NoGap -> "ng"
      displayNumber = \case
        Singular -> "s"
        Plural -> "p"


  in displayNumber n <> ", " <> displayGap g

syntacticCategories :: [Syn]
syntacticCategories =
  NP sing : S sing : S (Features {number = Singular, gap = Gap}) : N sing : NP (Features {number = Singular, gap = Gap}) : NP plur : N plur : []
--   liftA2 (:/:) (take 6 syntacticCategories) (take 6 syntacticCategories)
--   <> liftA2 (:\:) (take 6 syntacticCategories) (take 6 syntacticCategories)

data Node a = Leaf Text Syn | Node a a deriving stock (Functor, Show)
type Tree = FreeT Node [] Void
type Idiom = FreeF Node Void (FreeT (Compose [] (FreeF Node Void)) Identity Syn)
type PartialTree = FreeT Node [] (Maybe Void)
type Parser = ParsecT Void Text [] (Fix Node)
type Unfolder s = s
    -> Compose
     []
     (FreeF Node Void)
     (FreeT (Compose [] (FreeF Node Void)) Identity s)
type Folder t = Base PartialTree t -> t

sing, plur :: Features
sing = Features {number = Singular, gap = NoGap}
plur = Features {number = Plural, gap = NoGap}

pattern Sing :: Features
pattern Sing <- Features {number = Singular}
pattern Plur :: Features
pattern Plur <- Features {number = Plural}
