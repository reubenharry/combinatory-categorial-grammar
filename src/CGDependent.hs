{-# LANGUAGE TypeOperators, PatternSynonyms, TypeFamilies, FlexibleContexts, TypeApplications, ScopedTypeVariables,
    GADTs, AllowAmbiguousTypes, DataKinds, TemplateHaskell, StandaloneKindSignatures, PolyKinds #-}
module CGDependent where
import Prelude hiding (words, Word)
import Control.Applicative (Applicative (..), Alternative (empty, (<|>)))
import qualified Data.Text as T 
import Data.Text (Text, pack)
import Text.Megaparsec ( (<|>), empty, runParser, errorBundlePretty, Parsec, MonadParsec(try, eof) )
import Text.Megaparsec.Char (string, space)
import Text.Megaparsec.Char.Lexer (lexeme)
import Data.Kind (Type)
import Data.Singletons.TH ( Sing, genSingletons, Void )

data Category =  S | NP | N | BS Category Category | FS Category Category
genSingletons [''Category]

type family Semantic e (s :: Category) where    Semantic e NP  = e
                                                Semantic _ S = Bool
                                                Semantic e N = (e -> Bool)
                                                Semantic e (FS a b) = (Semantic e b -> Semantic e a)
                                                Semantic e (BS a b) = (Semantic e b -> Semantic e a)

type Lexicon e = forall (a :: Category). Syntactic a -> [(Parser Text Text, Semantic e a)]

data MultiTree a e =      forall b . Backward [(MultiTree b e, MultiTree (BS a b) e)]
                        | forall b . Forward  [(MultiTree (FS a b) e, MultiTree b e)] 
                        | Word [(Parser Text Text, Semantic e a)]

language :: Int -> Lexicon e ->  Syntactic a -> [MultiTree a e]
language i lexicon a | i==0 = [] | otherwise = [ Word (lexicon a),
    Backward (liftA2 (,) (language (i-1) lexicon NounPhrase) (language (i-1) lexicon (a `BackSlash` NounPhrase))),
    Forward (liftA2 (,) (language (i-1) lexicon (a `ForwardSlash` NounPhrase)) (language (i-1) lexicon NounPhrase)),
    Backward (liftA2 (,) (language (i-1) lexicon Sentence) (language (i-1) lexicon (a `BackSlash` Sentence))),
    Forward (liftA2 (,) (language (i-1) lexicon (a `ForwardSlash` Sentence)) (language (i-1) lexicon Sentence)),
    Backward (liftA2 (,) (language (i-1) lexicon Noun) (language (i-1) lexicon (a `BackSlash` Noun))),
    Forward (liftA2 (,) (language (i-1) lexicon (a `ForwardSlash` Noun)) (language (i-1) lexicon Noun)),
    Forward (liftA2 (,) (language (i-1) lexicon (a `ForwardSlash` Adjective)) (language (i-1) lexicon Adjective))]

interpreter :: MultiTree a e -> Parser Text (Semantic e a)
interpreter = \case Word ws -> foldr (|||) empty [lexeme space a >> pure s | (a,s) <- ws]
                    Forward ab_b -> do (ab, b) <- sequenceinterpreters ab_b; return $ ab b
                    Backward a_ba -> do (a, ba) <- sequenceinterpreters a_ba; return $ ba a
    where sequenceinterpreters xys = let alts = foldr (|||) empty . fmap interpreter; (xs,ys) = unzip xys 
                                     in liftA2 (,) (alts xs) (alts ys)

interpret :: forall (e :: Type) . forall (c :: Category) . Lexicon e -> Text -> Syntactic c -> Semantic e c
interpret lexicon t cat = let parser = foldr (|||) empty $ (<* eof) . interpreter @_ @e <$> language 3 lexicon cat
    in (either (error . errorBundlePretty) id . runParser parser "") t




---------------------------------
-- useful synonyms
---------------------------------
pattern NounPhrase = SNP
pattern Sentence = SS
pattern Noun = SN
pattern ForwardSlash a b = SFS a b
pattern BackSlash a b = SBS a b
pattern VerbPhrase = Sentence `BackSlash` NounPhrase
pattern TransitiveVerb = (Sentence `BackSlash` NounPhrase) `ForwardSlash` NounPhrase
pattern Determiner = NounPhrase `ForwardSlash` Noun
pattern Adjective = Noun `ForwardSlash` Noun
(|||) :: MonadParsec e s f => f a -> f a -> f a
(|||) x y = try x <|> y
type Syntactic a = Sing a
type Parser a b = Parsec Void a b
