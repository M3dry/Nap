module Static.Test where

import Data.Map qualified as M
import Data.Text.Lazy qualified as T
import Parser.Pattern (patternP)
import Parser.Type (Type (TComplex, TSimple, TVar), TypeDef (tConstructors, tName, tVars), typeDefP)

-- import Static.Typing (patternTypedRefs)

import Static.Typing (typePattern)
import Text.Parsec (parse)
import Text.Pretty.Simple (pShow)

unwrap x = case x of
    Right r -> r
    Left err -> error . T.unpack $ pShow err

pat pat' = unwrap $ parse patternP "" pat'

opt = unwrap $ parse typeDefP "" "type Opt(a) = { Just(a), None(), }"
list = unwrap $ parse typeDefP "" "type List(a) = { Cons(a, List(a)), Empty(), }"

constructors ts =
    M.fromList $
        concatMap
            ( \t ->
                map
                    (\(name, sig) -> (name, (sig, TComplex (tName t) $ map TVar $ tVars t)))
                    $ tConstructors t
            )
            ts

-- run = putStrLn . T.unpack . pShow $ patternTypedRefs (TComplex "Opt" [TComplex "Vector" [TSimple "Int"]]) (pat "Just([100])") (constructors [opt])
run :: IO ()
run = putStrLn . T.unpack . pShow $ typePattern (pat "Cons(x, xs)") (constructors [opt, list])
