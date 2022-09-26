{-# LANGUAGE BlockArguments #-}

module ChessLexicon where

import qualified Prelude as P
import CGDependent
import Data.Text (Text)
import Relude


data ChessEntity = Square File Rank | Piece Pieces Color deriving Show

data Pieces = Bishop | Rook deriving Show
data Color = White | Black deriving Show

data File = A | B | C deriving Show
data Rank = One | Two | Three deriving Show

chessLexicon :: Lexicon ChessEntity
chessLexicon = \case
    NounPhrase -> [
        ("a1", Square A One)
        ]

    Sentence `BackSlash` NounPhrase -> [
        ("exists", const True)
        ]

    (Sentence `BackSlash` Sentence) `ForwardSlash` Sentence ->
        [
            ("and", (&&))
        ]

    Adjective -> [
        ("white", toAdj \case
            Piece _ White -> True
            _ -> False),
        ("black", toAdj \case
            Piece _ Black -> True
            _ -> False)
        ]

    VerbPhrase `ForwardSlash` Adjective -> [
        ("is", \f -> f (const True))
        ]

    _ -> []


toAdj :: Applicative f => f Bool -> f Bool -> f Bool
toAdj = liftA2 (&&)

-- main = interpret @ChessEntity lexicon "John runs" Sentence


