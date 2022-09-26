
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CGDependentFancy where

import Prelude hiding (words, Word)
import Control.Applicative (Applicative (..), Alternative (empty, (<|>)), Const (Const))
import Data.Text qualified as T
import Data.Text (Text, pack)
import Text.Megaparsec
    ( empty,
      (<|>),
      runParser,
      errorBundlePretty,
      Parsec,
      MonadParsec(try, eof), ParseErrorBundle, some )
import Text.Megaparsec.Char (string, space)
import Data.Kind (Type)
import Text.Megaparsec.Char.Lexer (lexeme)
import Data.Singletons ( Sing )
import Data.Singletons.TH ( Void, genSingletons )
import Data.Foldable (asum)
import Control.Monad.Reader (Reader, join, runReader, ReaderT (runReaderT), MonadReader (ask), asks)
import Control.Monad.Except
    ( guard,
      join,
      MonadError(throwError),
      MonadTrans(lift),
      runExcept,
      Except )
import qualified Data.Text.IO as T
import Data.Maybe (isJust)
import Data.Either (rights)
import Data.Functor.Foldable.TH ()
import Data.Bifunctor (Bifunctor(bimap, second))
import Data.Functor.Combinator (HFunctor(..))
import Control.Monad.State (StateT, execStateT, evalStateT, modify, MonadState (get, put))
import Debug.Trace (traceM)
import Control.Monad.Cont (ContT (runContT, ContT), filterM, when)
import Relude (whenM, allM, IsString)
import Data.List (singleton)

-- todo:
-- faster parser + full features
-- automatical morphology
-- automatic parsing of nouns
-- monadic entity type
-- (co)free (co)monadic (co)folds

newtype Features = Number Bool


data Syn = N Features | NP Features | S | BS Syn Syn | FS Syn Syn
-- macro to generate singletons, needed for code below where value depends on syntactic category
genSingletons [''Bool]
genSingletons [''Features]
genSingletons [''Syn]

type M e w = ContT Bool (ReaderT (WorldState e w) (Except Text))
runM :: WorldState e w -> M e w Bool -> Either Text Bool
runM a = runExcept . flip runReaderT a . flip runContT return

data WorldState e w = WorldState {world :: w, entities :: [e]}


-- | a semantic type, given a syntactic category
type family Sem (s :: Syn) (e :: Type) (w :: Type) where
    Sem ('NP _) e _ = e
    Sem 'S _ _ = Bool
    Sem ('N _) e _ = (e -> Bool)
    Sem ('FS a b) e w = (Sem b e w -> M e w (Sem a e w))
    Sem ('BS a b) e w = (Sem b e w -> M e w (Sem a e w))

newtype Lexicon e w  = Lexicon {runLexicon :: forall a. Sing a -> MultiBranching e w Sing a}
-- base functor of a tree
data Branching e w (f :: Syn -> Type) (a :: Syn)  where
    Backward :: (f c, f (BS a c))  ->  Branching e w f a
    Forward :: (f (FS a c), f c)  ->  Branching e w f a
    Word :: (Parser Text Text, Sem a e w)-> Branching e w f a


-- combinator for building more complex base functors
type ComposeH tr t e w f a = tr (t e w f a)
-- base functor for multitree
newtype MultiBranching e w f a = Options (ComposeH (StateT Int []) Branching e w f a)
instance Semigroup (MultiBranching e w f a) where
    (Options a) <> (Options b) = Options (lift $ evalStateT a 0 <> evalStateT b 0)

-- fix point of the base functor
type MultiTree e w = Fix (MultiBranching e w)
type Tree e w = Fix (Branching e w)



-- needed for recursion schemes because of the fancier kinds of the Fix operator

instance HFunctor (Branching e w) where
    hmap _ (Word x) = Word x
    hmap f (Forward (a,b)) = Forward (f a, f b)
    hmap f (Backward (a,b)) = Backward (f a, f b)

instance HFunctor (MultiBranching e w) where
    hmap f (Options a) = Options $ fmap (hmap f) a

instance MonadError e m => MonadError e (ContT a m) where
    throwError = lift . throwError


-------------
-- recursion schemes
-- needed to be redefined because of their new kinds
-------------

ana :: HFunctor f => (forall a g . g a -> f g a) -> (g x -> Fix f x)
ana coalg = Fix . hmap (ana coalg) . coalg

cata :: HFunctor f => (forall a . f g a -> g a) -> Fix f x ->  g x
cata alg = alg . hmap (cata alg) . unFix

postpro :: HFunctor f => (forall x g. f g x -> f g x) -> (forall a . g a -> f g a) -> (g x -> Fix f x)
postpro e coalg = Fix . hmap (hoist e . postpro e coalg) . coalg
    where
        hoist :: HFunctor f => (forall g x. f g x -> f g x) -> Fix f x -> Fix f x
        hoist n = cata (Fix . n)


----------
-- the parsing algebra
----------

data Interpreter e w (a :: Syn) where
    Interpreter :: {unInterpreter :: Parser Text (M e w (Sem a e w))} -> Interpreter e w a


evaluate :: MultiBranching e w (Interpreter e w) a -> Interpreter e w a
evaluate (Options ws) = Interpreter $ foldr (\x y -> try x <|> y) empty $ do
           ws' <- evalStateT ws 0
           case ws' of
                Word (a,s) -> return (lexeme space a >> pure (pure s))
                Forward (Interpreter a, Interpreter b) -> singleton do
                    a' <- a; b' <- b; return $ join (a' <*> b')
                Backward (Interpreter a, Interpreter b) -> singleton do
                    a' <- a; b' <- b; return $ join (b' <*> a')


----------------
-- the interpreter
----------------

interpretFancy :: 
    forall a e w. 
    Lexicon e w -> 
    Syntactic (a :: Category) -> 
    Text -> 
    Semantic e w (a :: Category)

interpretFancy lexicon cat = 
    either (throwError . T.pack . errorBundlePretty) id . -- push parse errors into M
    
    parse ((cata evaluate . 
            postpro transform (runLexicon (genericLexicon <> lexicon <> ruleLexicon)) ) 
            cat)
    where
    parse p = runParser (unInterpreter p <* eof) ""
    transform (Options l) = Options do
        depth <- get -- cap the max sentence depth, because my parser is weak currently
        modify (+1)
        guard (depth <= 2)
        l

-- the interpreter is a hylomorphism
run :: forall e w a . Lexicon e w -> WorldState e w -> Text -> IO ()
run lexicon w  =
    either T.putStrLn print .
    runM w .
    interpretFancy lexicon Sentence

-- runExample ::
--     WorldState ExampleEntity ExampleWorld -> Text -> IO ()
-- runExample w =
--     either T.putStrLn print .
--     interpretFancy @ExampleEntity @ExampleWorld
--         (genericLexicon <> exampleLexicon <> ruleLexicon) w
        

-- runChess :: 
--     WorldState ChessEntity ChessWorld -> Text -> IO ()
-- runChess w  =
--     either T.putStrLn print .
--     interpret @ChessEntity @ChessWorld
--         (genericLexicon <> chessLexicon <> ruleLexicon) w
        

options :: [Branching e w f a] -> MultiBranching e w f a
options = Options . lift

pattern Sentence = SS
pattern ForwardSlash a b = SFS a b
pattern BackSlash a b = SBS a b
pattern NounPhrase x = SNP x
pattern Noun x = SN x
pattern VerbPhrase x = Sentence `BackSlash` (NounPhrase x)
pattern TransitiveVerb x y = (VerbPhrase x) `ForwardSlash` (NounPhrase y)
pattern Determiner x y = (SFS (SNP x) (SN y))
pattern Adjective x y = SFS (SN x) (SN y)

pattern Singular = SNumber STrue
pattern Plural = SNumber SFalse



ruleLexicon :: Lexicon e w
ruleLexicon = Lexicon \case
    cat -> options $ [
        (`backward` NounPhrase Singular),
        (`backward` NounPhrase Plural),
        (`backward` Sentence),
        (`backward` Noun Singular),
        (`backward` Noun Plural),
        (`forward` NounPhrase Singular),
        (`forward` NounPhrase Plural),
        (`forward` Sentence),
        (`forward` Noun Singular),
        (`forward` Noun Plural),
        (`forward` Adjective Singular Singular)] <*> pure cat

    where
        forward x y = Forward (x `ForwardSlash` y, y)
        backward x y = Backward (y, x `BackSlash` y)

        

genericLexicon :: forall e w. Lexicon e w
genericLexicon = Lexicon \case

    Sentence -> options [
            Word ("Yes",  True)
            ]

    Determiner Singular Singular -> options [
        Word ("the", \p -> do

            ents <- asks entities
            case filter p ents of
                [x] -> pure x
                [] -> throwError "No such entities"
                _ -> throwError "Many such entities"

            ),
        Word ("every", \p -> do

            ents <- asks entities
            ContT (\f -> do
                let filtered = filter p ents
                when (null filtered) (throwError "No such entities")
                allM f filtered
                )

            )
        ]

    Determiner Plural Plural -> options [
        Word ("the", \p -> do

            ents <- asks entities
            ContT (\f -> do
                let filtered = filter p ents
                when (null filtered) (throwError "No such entities")
                allM f filtered
                )

            )

        ]

    (VerbPhrase _) `ForwardSlash` (Adjective _ _) -> options [
        Word ("is", \n -> (return .) <$> n (const True))
        ]


    _ -> options []




data ChessEntity = Square File Rank | Piece Pieces Color deriving Show

data Pieces = Bishop | Rook deriving Show
data Color = White | Black deriving Show

data File = A | B | C deriving Show
data Rank = One | Two | Three deriving Show

type ChessWorld = (File, Rank) -> Maybe Pieces

w1 = \case
        (A, One) -> Just Bishop
        _ -> Nothing

chessworld1 :: WorldState ChessEntity ChessWorld
chessworld1 = WorldState {
    world = w1,
    entities = rights $ maybeToEither <$> join [[flip Piece White <$> w1 square, Just $ Square f r] | square@(f,r) <- squares]}

maybeToEither = \case
    Just x -> Right x
    Nothing -> Left ""
squares = liftA2 (,) [A,B,C] [One, Two, Three]

showFile :: File -> Text
showFile = \case
    A -> "a"
    B -> "b"
    C -> "c"

showRank :: Rank -> Text
showRank = \case
    One -> "1"
    Two -> "2"
    Three -> "3"

chessLexicon :: Lexicon ChessEntity ChessWorld
chessLexicon = Lexicon \case

    NounPhrase _ -> options [
        -- a1, a2...
        Word (string (showFile file <> showRank rank),  Square file rank)
        | (file, rank) <- squares
        ]

    VerbPhrase _ -> options [

        Word ("isOccupied", \case
                Square file rank  -> do
                    board <- asks world
                    pure $ isJust $ board (file, rank)
                x -> throwError $ (T.pack . show) x <> " is not a square")
        ]

    Noun number -> options [
        Word (case number of Singular -> "piece"; Plural -> "pieces",  \case
            Square _ _ -> False
            _ -> True),

        Word (case number of Singular -> "rook"; Plural -> "rooks", \case
            Piece Rook _ -> True
            _ -> False
             ),

        Word (case number of Singular -> "bishop"; Plural -> "bishops", \case
            Piece Bishop _ -> True
            _ -> False
             )
        ]
    Adjective _ _ -> options [
        Word ("white", toAdj \case
            Piece _ White -> True
            _ -> False),
        Word ("black", toAdj \case
            Piece _ Black -> True
            _ -> False)
        ]
    _ -> options []

toAdj f p = pure $ liftA2 (&&) f p




type ExampleEntity = Text
type ExampleWorld = Int

exampleworld1 :: WorldState ExampleEntity ExampleWorld
exampleworld1 = WorldState {
    world = 1 :: Int,
    entities = ["John", "Jane"]
    }

newtype Lemma = Lemma Text deriving newtype (IsString, Show)

morphology :: Syntactic (c :: Category) -> Lemma -> Parser Text Text
morphology (Noun Singular) = \case
    Lemma x -> fail "No parse" -- string x
morphology (Noun Plural) = \case
    Lemma "child" -> "children"
    Lemma "man" -> "men"
    Lemma x -> fail "No parse" -- string $ x <> "s"
morphology _ = const $ fail "No parse"

exampleLexicon :: Lexicon ExampleEntity ExampleWorld
exampleLexicon = Lexicon \case

    NounPhrase Singular -> options [
        Word ("John",   "John"),
        Word ("Jane",   "Jane")
        -- Word (T.concat <$> some "a", )
        ]

    -- Sentence `ForwardSlash` NounPhrase -> options [
    --     Word ("Behold:", const $ pure True)

    --     ]

    VerbPhrase number -> options [
            Word (case number of Singular -> "runs"; Plural -> "run", const $ pure False)
        ]

    TransitiveVerb _ _ -> options [
        Word ("sees",  const $ pure $ const $ pure True)
        ]

    s@(Noun features) -> options [
        Word (morphology s "man",  const False),
        Word ("children", (=="John"))
        
        ]



    Adjective _ _ -> options [
        Word ("happy", pure)
        ]
    _ -> options []



instance Semigroup (Lexicon e w) where
    (Lexicon l1) <> (Lexicon l2) = Lexicon $ liftA2 (<>) l1 l2

-- like the normal type level fixpoint, but parametrized by a type of kind Syn
data Fix (g :: (Syn -> *) -> Syn -> *) (a :: Syn) where
    Fix :: {unFix :: g (Fix g) a} -> Fix g a


type Parser a b = Parsec Void a b

type Syntactic = Sing
type Category = Syn
type Semantic e w a = M e w (Sem a e w)



-- data FragmentF e w g (f :: Syn -> Type) a = Continue (MultiBranching e w f a) | Pause (g a)
-- type Fragment e w g = Fix (FragmentF e w g)

-- data Lexicon' e w = Lexicon' (forall x. Sing x -> MultiBranching e w (Fragment e w SSyn) x)
-- exampleLexicon' :: Lexicon' ExampleEntity ExampleWorld
-- exampleLexicon' = Lexicon' \case

--     NounPhrase -> options [
--         Word ("John",   "John"),
--         Word ("Jane",   "Jane"),
--         Forward  
--             (Fix (Pause (SFS NounPhrase SNP)), 
--             Fix $ Continue $ options [
--                 Forward 
--                     (Fix $ Continue $ options [
--                         Word @(FS NP NP) ("Mr", const $ pure "")] , 
--                     Fix $ Continue $ options [
--                         Word ("b", "John")])])
--         ]

-- -- gana :: HFunctor f => (forall x. SSyn x -> f (Fragment e w SSyn) x) -> (SSyn x -> Fix f x)
-- gana :: HFunctor f => (Sing x -> f (Fragment e w SSyn) x) -> Sing x -> Fix f x
-- gana coalg = a . Fix . Pause . coalg where

--     a :: HFunctor f => Fix (FragmentF e w (f (Fragment e w SSyn))) x -> Fix f x
--     a = Fix . hmap ( a . emap coalg . collapse) . distribute

--     collapse x = x `bind` id

--     Fix (Pause a) `bind` f = f a
--     -- Fix (Continue m) `bind` f = Fix (Continue (hmap (`bind` f) m))

-- -- generalizedUnfold :: GeneralizedGrammar -> Category -> MultiTree
-- -- generalizedUnfold f = a . In . Pause . f where

-- --     a = In . fmap (a . emap f . collapse) . distribute


--     emap :: (SSyn x -> f (Fragment e w SSyn) x)
--         -> Fix (FragmentF e1 w1 SSyn) x1
--         -> Fix (FragmentF e w (f1 (Fragment e w SSyn))) x1
--     emap f = go where
--         go (Fix (Pause a))  = Fix (Pause (undefined a)) -- Fix $ Pause (f a)
--         -- go (Fix (Continue fa)) = Fix $ Continue (go `undefined` fa)

--     distribute :: HFunctor f => Fix (FragmentF e w (f (Fragment e w SSyn))) x
--         -> f (Fix (FragmentF e w (Fix (FragmentF e w SSyn)))) x
--     distribute (Fix (Pause fx)) = hmap undefined fx -- Fix . (Pause `undefined` fx)
--     -- distribute (Fix (Continue ff)) = Fix . Continue . (distribute `undefined` ff)


-- ex :: SSyn a -> MultiBranching ExampleEntity ExampleWorld (Fragment ExampleEntity ExampleWorld Sing) a
-- ex SS = options [
--     Forward 
--         (Fix (Pause (SFS SS SNP)), 
--         Fix $ Continue $ options [
--             Forward 
--                 (Fix $ Continue $ options [
--                     Word @(FS NP NP) ("Mr", const $ pure "")] , 
--                 Fix $ Continue $ options [
--                     Word ("b", "John")])])]



-- run chessLexicon chessworld1 "every piece is white"