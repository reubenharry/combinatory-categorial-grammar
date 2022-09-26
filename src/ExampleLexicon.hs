
module ExampleLexicon where 

import qualified Prelude as P
import CGDependent  
import Data.Text (Text)
import Relude


type ExampleEntity = Text

exampleLexicon :: Lexicon ExampleEntity
exampleLexicon = \case
    NounPhrase -> [
        ("John", "JOHN"),
        ("Jane", "JANE")
        ]

    Sentence `BackSlash` NounPhrase -> [
        ("runs", \x -> x `elem` ["JOHN"])
        ]


    NounPhrase `ForwardSlash` NounPhrase -> [
        ("Mr", \x -> x)
        ]

    (Sentence `BackSlash` Sentence) `ForwardSlash` Sentence ->
        [
            ("and", (&&))
        ]

    _ -> []

-- main = interpret @ExampleEntity exampleLexicon "John runs" Sentence 


-- interpret @ExampleEntity exampleLexicon "John" NounPhrase
-- interpret @ExampleEntity exampleLexicon "John runs" Sentence
-- interpret @ExampleEntity exampleLexicon "runs" VerbPhrase
-- interpret @ExampleEntity exampleLexicon "blah" VerbPhrase
-- interpret @ExampleEntity exampleLexicon "and" ((Sentence `BackSlash` Sentence) `ForwardSlash` Sentence)
-- interpret @ExampleEntity exampleLexicon "and John runs" (Sentence `BackSlash` Sentence)