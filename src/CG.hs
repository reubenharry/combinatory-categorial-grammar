{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module CG where

import Relude hiding (fold)
-- import Control.Applicative (Applicative (..), Alternative (empty, (<|>)))
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
import Prelude hiding (elem, undefined)
import Control.Monad.Logic
import Control.Arrow ((***))
import qualified Data.Map.Strict as M
import Data.Functor.Classes (Eq1 (liftEq), Show1)



-- Syn is the type of syntactic categories
data Syn = NP | S | N | Syn :/: Syn | Syn :\: Syn deriving (Show, Eq, Ord)


-- this is where we state all terminal rules
leaf :: Syn -> [Text]
leaf = fromMaybe [] . flip M.lookup leafs

leafs :: M.Map Syn [Text]
leafs = M.fromList [
  (NP , ["a1", "a2"]),
  (N :/: N , ["white", "black"]),
  (S :\: NP , ["exists"]),
  ((S :/: NP) :\: NP , ["attacks"]),
  (N , ["bishop", "piece", "rook"]),
  (NP :/: N , ["the", "every", "a"])
  ]



-- we state idioms here
idioms :: Syn -> [Idiom]
idioms = \case
  N -> [
    branch
      (word "key" (N :/: N))
      (word "piece" N)
    ]

  (S :\: NP) ->
    [
      branch
        (word "defends" NP)
        (branches
          (syn NP)
          (branches
            (word "with" NP)
            (syn NP)))
    ]

  _ -> []



--------------
-- the grammar
-- and 
-- the parser
-------------

-- a -> b (a\b)
-- a -> (a/b) b
grammarRules :: Syn -> [Node Syn]
grammarRules syn1 = do
  syn2 <- take 3 syntacticCategories
  return (Node S (syn1 :/: syn2) syn2) <|> return (Node S syn2 (syn1 :\: syn2))


unfolder :: Unfolder Syn
unfolder syn = Compose $
  (fmap . fmap) pure (Free . flip Leaf syn <$> leaf syn)
  <> idioms syn
  <> (fmap . fmap) pure (Free <$> grammarRules syn)

viewer :: Folder [Text]
viewer =
  foldr combine empty
  . fmap (\case
      Pure (Just a) -> absurd a
      Pure Nothing -> Node S empty empty
      Free x -> x
      )
  . getCompose
  where
    combine (Node _ a b) c = liftA2 (\x y -> "(" <> x <> " " <> y <> ")") a b <|> c
    combine (Leaf a _) b = pure a <|> b

language :: Syn -> MultiTree
language = gpostpro (distGFutu (fmap Identity . runIdentity)) id unfolder

grammar :: Syn -> IO ()
grammar =
  mapM_ T.putStrLn
  . fold viewer
  . cutoff 5
  . language


parser :: Folder Parser
parser =
    let combine (Node _ a b) c = try (do  x <- a; string " " *> (Fix . Node S x <$> b)) <|> c
        combine (Leaf a syn) b = try (Fix . flip Leaf syn <$> string a ) <|> b
    in foldr combine empty
        . fmap (\case
              Pure (Just a) -> absurd a
              Pure Nothing -> Node S empty empty
              Free x -> x)
        . getCompose


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

-- bar = scanl1 (+) [1,2,3]

stepUp' :: [Fix Node] -> [Fix Node]
stepUp' = foldr foo [] where
  foo newSyn (oldSyn:ss) = case combine (newSyn, oldSyn) of
    Just b -> b:ss --  undefined $ zip syns (Prelude.tail syns)
    Nothing -> newSyn:oldSyn:ss
  foo newSyn [] = [newSyn]
  -- foo = undefined
  combine (l1 , l2) = 
    let s1 = getSyn l1
        s2 = getSyn l2 
        in case (s1,s2) of
          (a, b :\: c) | a==c -> Just (Fix $ Node b l1 l2 )
          (b :/: c, a) | a==c -> Just (Fix $ Node b l1 l2 )
          _ -> Nothing


-- getSyn :: t0 -> a0
getSyn = \case
  Fix (Node syn _ _) -> syn
  Fix (Leaf _ syn) -> syn

stepUp :: [Syn] -> [Syn]
stepUp = foldr foo [] where
  foo newSyn (oldSyn:ss) = case combine (newSyn, oldSyn) of
    Just b -> b:ss --  undefined $ zip syns (Prelude.tail syns)
    Nothing -> newSyn:oldSyn:ss
  foo newSyn [] = [newSyn]
  combine (a, b :\: c) | a==c = Just b
  combine (b :/: c, a) | a==c = Just b
  combine _ = Nothing

instance Eq1 ( Node) where
  liftEq eq (Leaf t1 s1) (Leaf t2 s2) = (t1==t2 && s1==s2)
  liftEq eq (Node s1 a b) (Node s2 c d) = eq a c && eq b d && s1 == s2
  liftEq eq _ _ = False
instance Show1 ( Node) where
  -- liftShowsPrec (Int -> a -> ShowS) ([a] -> ShowS) Int (f a) String

idem f x = let y = f x in if y == x then x else idem f y

isS (Fix (Node S _ _)) = True
isS _ = False

-- bottomUpParse :: Text -> [[Syn]]
-- bottomUpParse = fmap (linearize . Prelude.head) . fmap snd . filter (isS . Prelude.head . fst) . uncurry zip . 

bottomUpParse = fmap (fmap (linearize)) . fmap (idem stepUp') . mapM leafMeaning . T.words

leafMeaning :: Text -> [Fix Node]
leafMeaning text = Fix . Leaf text <$> filter (\x -> text `elem` leaf x) (M.keys leafs)





--------------
-- helper code
--------------

linearize :: Fix Node -> (Text, Text)
linearize = cata \case
  Node _ (a,s1) (b,s2) -> (
    "(" <> a <> T.pack (replicate (20 - T.length a) ' ') <> b <> ")",
    " " <> s1 <> T.pack (replicate (20 - T.length s1) ' ') <> s2 <> " "
    )
  Leaf a s -> ("  " <> a, pack $ "  " <> display s)

display :: Syn -> String
display = \case
  NP -> "NP"
  S -> "S"
  N -> "N"
  a :/: b -> "(" <> display a <> "/" <> display b <> ")"
  a :\: b -> "(" <> display a <> "\\" <> display b <> ")"


syntacticCategories :: [Syn]
syntacticCategories = [
  NP, S, N, NP :/: S, NP :/: N, NP :/: NP, S :/: S, S :/: NP, S :/: N, N :/: N, N :/: NP, N :/: S,
  NP :\: S, NP :\: N, NP :\: NP, S :\: S, S :\: NP, S :\: N, N :\: N, N :\: NP, N :\: S, (S :/: NP) :\: NP ]
  -- pure NP <|> pure S >>- \x -> pure ((:/:) x x)
  -- liftA2 (:/:) syntacticCategories syntacticCategories
  -- `interleave` liftA2 (:\:) syntacticCategories syntacticCategories

data Node a = Leaf Text Syn | Node Syn a a deriving stock (Functor, Show)
type MultiTree = FreeT Node [] Void
type Idiom = FreeF Node Void (FreeT (Compose [] (FreeF Node Void)) Identity Syn)
type PartialTree = FreeT Node [] (Maybe Void)
type Parser = ParsecT Void Text [] (Fix Node)
type Unfolder s = s
    -> Compose
     []
     (FreeF Node Void)
     (FreeT (Compose [] (FreeF Node Void)) Identity s)
type Folder t = Base PartialTree t -> t

syn = FreeT . Identity . Pure
branch a b = Free $ Node S a b
branches a b = FreeT $ Identity $ Free $ Compose [Free (Node S a b)]
word lemma s = FreeT . Identity . Free . Compose $ [Free (Leaf lemma s)]

--------------
--
--------------

