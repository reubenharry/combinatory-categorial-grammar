
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE TypeOperators #-}

-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- -- {-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE BlockArguments #-}

-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- -- {-# LANGUAGE IncoherentInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE DataKinds #-}




module CGTyped () where

-- import Control.Applicative (Applicative (..), Alternative (empty, (<|>)))
-- import Data.Text qualified as T
-- import Data.Functor.Foldable
--     ( hylo,
--       Corecursive(ana, gpostpro),
--       Recursive(cata, gprepro),
--       Base,
--       unfold,
--       futu,
--       gfutu,
--       distGFutu,
--       gcata,
--       gana,
--       fold )
-- import Data.Functor.Compose (Compose (..))
-- import Control.Monad.Trans.Free (FreeT (FreeT), FreeF (Pure, Free), cutoff, free)
-- import Data.Void (Void, absurd)
-- import Text.Megaparsec
--     ( empty,
--       (<|>),
--       runParserT,
--       errorBundlePretty,
--       MonadParsec(eof, try),
--       ParsecT, runParser, Parsec )
-- import Text.Megaparsec.Char (string, space)
-- import Data.Text (Text, pack)
-- import Data.Fix (Fix(..))
-- import qualified Data.Text.IO as T
-- import Control.Monad.Identity ( Identity(Identity, runIdentity), join )
-- import Data.Functor (($>))
-- import Data.Kind (Type)
-- import Data.Data (Proxy (Proxy))
-- import Data.List (intersperse)
-- import Text.Megaparsec.Char.Lexer (lexeme)
-- import Prelude


-- data NP = NP deriving Show
-- data S = S deriving Show
-- data N = N

-- data a :\: b = a :\: b
-- data a :/: b = a :/: b

-- type Entity = Text

-- -- the crucial homomorphism property needed for compositionality
-- type family (Sem (a :: Type)) :: Type where
--     Sem NP = Entity
--     Sem S = Bool
--     Sem N = Entity -> Bool
--     Sem (a :\: b) = Sem b -> Sem a
--     Sem (a :/: b) = Sem b -> Sem a


-- type VerbPhrase = S :\: NP
-- type Sentence = S
-- type NounPhrase = NP
-- type Adjective = N :/: N
-- type TransitiveVerb = VerbPhrase :/: NP
-- type Determiner = NP :/: N
-- type Noun = N


-- type EntriesFor a = [(Text, Sem a)]
-- data Lexicon a =
--           forall b . Backward [(Lexicon b, Lexicon (a :\: b))]
--         | forall b. Forward [(Lexicon (a :/: b), Lexicon b)]
--         | Word (EntriesFor a)


-- -- foo :: Proxy a -> Text -> Sem a
-- -- foo (_ :: Proxy NP) "John" = "John"

-- lexicon :: Lexicon Sentence
-- lexicon =

--     let pn :: EntriesFor NounPhrase
--         pn = [
--             ("John", "John"),
--             ("Johnny", "John"),
--             ("Jane", "Jane")
--                 ]

--         det :: EntriesFor Determiner
--         det = [
--             ("the", undefined . flip filter ["John", "Jim", "Jane"])
--             ]


--         noun :: EntriesFor Noun
--         noun = [
--              ("woman", (`elem` ["Jane"])),
--              ("man", (`elem` ["John"])),
--              ("person", const True)
--             ]


--         adj :: EntriesFor Adjective
--         adj = [
--             ("happy", \f x -> x == "John" && f x)
--             ]


--         tv :: EntriesFor TransitiveVerb
--         tv = [
--                  ("likes", curry (`elem` [])),
--                  ("sees", curry (`elem` [("Jane", "John")]))
--             ]
--         iv :: EntriesFor VerbPhrase
--         iv = [
--             ("runs", (`elem` ["John"]))
--             ]

--         vp = [Word @VerbPhrase iv] <|> forward [Word @TransitiveVerb tv] np
--         np = [Word @NP pn] <> forward [Word @Determiner det] nouns
--         nouns = [Word @Noun noun] <|> forward [Word @Adjective adj] nouns
--         s = backward np vp

--     in undefined s
--         -- (liftA2 (,) undefined undefined <> iv)) -- ([Forward tv np] <> iv)
--     where
--     backward :: [Lexicon b] -> [Lexicon (a :\: b)] -> [Lexicon a]
--     backward a b = [Backward (liftA2 (,) a b)]
--     forward :: [Lexicon (a :/: b)] -> [Lexicon b] -> [Lexicon a]
--     forward a b = [Forward (liftA2 (,) a b)]

-- parser :: Lexicon a -> Parsec Void Text (Sem a)
-- parser (Word ws) = foldr (<|>) [lexeme space (string a) >> return s | (a,s) <- ws]
-- parser (Forward ab) = do
--     (a', b') <- sequenceParsers ab
--     return $ a' b'
-- parser (Backward ab) = do
--     (a', b') <- sequenceParsers ab
--     return $ b' a'

-- sequenceParsers ab = do
--     let (a,b) = unzip ab
--         parseAlternatives z = foldr1 (\x y -> try x <|> y) (parser <$> z)
--     a' <- parseAlternatives a
--     b' <- parseAlternatives b
--     pure (a', b')

-- parse :: Text -> IO ()
-- parse x = putStrLn $ either errorBundlePretty show $ runParser (parser lexicon) "" x

































-- data Entry = forall a . Show (Sem a) => Entry {
--     entry :: Proxy a -> Text -> Sem a
--     -- prints :: a
--     }

-- bar = case exampleLexicon !! 3 of
--     Entry x -> show (x Proxy "sees")

-- instance Show (a -> b) where
--     -- show _ = "function type"


-- -- 

-- exampleLexicon :: [Entry]
-- exampleLexicon = [
--     Entry @NP
--         \_ -> \case
--             "John" -> "John"
--             "Jane" -> "Jane"
--             _ -> error "unknown word"
--     ,
--     Entry @S
--         \_ -> \case
--             "Yes" -> True
--             "No" -> False
--             _ -> error "unknown word"
--     ,
--     Entry @VerbPhrase
--         \_ -> \case
--             "runs" -> (\x -> x `elem` ["John", "Jane"])
--             _ -> error "unknown word"
--     ,
--     Entry @TransitiveVerb
--         \_ -> \case

--             "sees" -> (\x y -> (x, y) `elem` [("John", "Jane"), ("John", "John")])
--             _ -> error "unknown word"
--     ,
--     Entry @Adjective
--         \_ -> \case
--             "happy" -> \f x -> x `elem` ["Jane", "John"] && f x
--             _ -> error "unknown word"


--     ]



-- interpret :: Lexicon a -> [Sem a]
-- interpret (Word ws) = snd <$> ws
-- interpret (Backward ab) = let (a,b) = unzip ab in (interpret =<< b) <*> (interpret =<< a)
-- interpret (Forward ab) = let (a,b) = unzip ab in (interpret =<< a) <*> (interpret =<< b)

-- linearize :: Lexicon a -> Text
-- linearize (Word ws) = T.concat $ intersperse "|" $ fst <$> ws
-- linearize (Backward ab) = let (a,b) = unzip ab in  helper a <> " " <> helper b
-- linearize (Forward ab) = let (a,b) = unzip ab in helper a <> " " <> helper b

-- helper b = T.concat (intersperse "|" (helper2 $ linearize <$> b))
-- helper2 [x] = [x]
-- helper2 y = fmap (\x -> "(" <> x <> ")") y
-- class HasGrammar a where
--     grammar :: a -> [(Text, Sem a)]

-- instance HasGrammar NP where
--     grammar NP = [("John", "John")]

-- -- -- -- instance HasGrammar (E :\: E) where
-- -- -- --     grammar _ = ["John", "Jane"]

-- -- instance HasGrammar S where
-- --     grammar S = []

-- -- instance HasGrammar (S :\: NP) where
-- --     grammar (S :\: NP) = [("runs", const False)]

-- -- instance HasGrammar (a :\: NP) where
-- --     grammar (a) = []

-- --     -- HasGrammar a => HasGrammar (a :\: E), 
-- -- data HasGrammar a where
-- --     Grammar :: [(Text, Sem a)] -> HasGrammar a

-- -- grammar (Grammar a) = a

-- -- produceLexicons :: forall a . HasGrammar a => a -> [Lexicon a]
-- -- produceLexicons a = (Word <$> grammar a) <> do
-- --         x <- produceLexicons NP -- Word <$> grammar E
-- --         y <- produceLexicons (undefined)

-- --         return $ Branches x y




-- -- class HasLexicon a where
-- --     lexicon :: Lexicon a -> Sem a

-- -- -- instance HasLexicon (T :\: E) where
-- -- --     lexicon (_, "runs") = const False -- (`elem` ["Jane"])

-- -- -- instance HasLexicon a => HasLexicon (a :\: b) where
-- -- --     lexicon (_,s) = const (lexicon @a (undefined, s)) --  (lexicon (undefined ,s))
-- -- instance HasLexicon NP where
-- --     lexicon (Word s) = s

-- -- instance HasLexicon (S :\: NP) where
-- --     lexicon (Word s) = (`elem` ["Jane"])
-- --     lexicon (Branches x y ) = lexicon y $ lexicon x

-- -- instance HasLexicon ((T :\: E) :\: E) where
-- --     lexicon (Word s) = curry (`elem` [("Jane", "John")])
--     -- lexicon (Branches x y) = lexicon y $ lexicon x

-- -- -- instance HasLexicon T where
-- -- --     lexicon (Branches x y) = lexicon y $ lexicon x



-- -- instance HasLexicon (a :\: E) => HasLexicon a where
-- -- --     lexicon (Word a@s) = word a
-- --     lexicon (Branches x y ) = lexicon y $ lexicon x

-- -- --     word (a, s) = error (show s)

-- -- -- interpret :: Lexicon T -> Sem T
-- -- -- interpret (Word a) = lexicon a
-- -- -- interpret (Branches x y) = lexicon x

-- -- -- interpret :: (HasLexicon a) => Lexicon a -> Sem a
-- -- -- interpret (Word a) = lexicon a
-- -- -- interpret (Branches x y) = (interpret y) (interpret x)

-- -- -- --------------
-- -- -- -- the grammar
-- -- -- -- and 
-- -- -- -- the parser
-- -- -- -------------

-- -- -- grammarRules :: Syn -> [Node Syn]
-- -- -- grammarRules syn1 = do
-- -- --   syn2 <- take 3 syntacticCategories
-- -- --   [Node (syn1 :/: syn2) syn2] <|> [Node syn2 (syn1 :\: syn2)]


-- -- -- unfolder :: Unfolder Syn
-- -- -- unfolder x = Compose $
-- -- --   (fmap . fmap) pure (Free . Word <$> Word x)

-- -- -- folder :: Folder [Text]
-- -- -- folder =
-- -- --   foldr combine empty
-- -- --   . fmap (\case
-- -- --       Pure (Just a) -> absurd a
-- -- --       Pure Nothing -> Node empty empty
-- -- --       Free x -> x
-- -- --       )
-- -- --   . getCompose
-- -- --   where
-- -- --     combine (Node a b) c = liftA2 (\x y -> "(" <> x <> " " <> y <> ")") a b <|> c
-- -- --     combine (Word a _) b = pure a <|> b

-- -- -- language :: Syn -> Lexicon
-- -- -- language = gpostpro (distGFutu (fmap Identity . runIdentity)) id unfolder

-- -- -- grammar :: Syn -> IO ()
-- -- -- grammar =
-- -- --   mapM_ T.putStrLn
-- -- --   . fold folder
-- -- --   . cutoff 5
-- -- --   . language