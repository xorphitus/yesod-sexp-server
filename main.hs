{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import           Yesod
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BL

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

data Sexp = List [Sexp] | Atom BL.ByteString | Symbol BL.ByteString deriving (Show, Eq)

toAtom :: (Show a) => a -> Sexp
toAtom = Atom . BL.pack . show

toSym :: String -> Sexp
toSym = Symbol . BL.pack

class (Show a) => Sexpable a where
  toSexp :: a -> Sexp
  toSexp = Atom . BL.pack . show

instance Sexpable Bool
instance Sexpable Int
instance Sexpable Integer
instance Sexpable Float
instance Sexpable Double
instance {-# OVERLAPPING #-} Sexpable String

instance Sexpable () where
  toSexp () = List []

instance (Sexpable a) => Sexpable [a] where
  toSexp xs = List (map toSexp xs)

instance (Sexpable a, Sexpable b) => Sexpable (a, b) where
  toSexp (x, y) = List [toSexp x, toSexp y]

instance (Sexpable a, Sexpable b, Sexpable c) => Sexpable (a, b, c) where
  toSexp (x, y, z) = List [toSexp x, toSexp y, toSexp z]

instance Sexpable Sexp where
  toSexp = id

cons :: Sexp -> Sexp -> Sexp
cons (Symbol x) (List xs)  = List ((Symbol x):xs)
cons (Atom x)   (List xs)  = List ((Atom x):xs)
cons (List xs)  (List xs') = List ((List xs):xs')
cons x          y          = List [x, y]

lispnize :: Sexp -> String
lispnize (List xs) = "(" ++ (intercalate " " (map lispnize xs)) ++ ")"
lispnize (Atom x) = BL.unpack x
lispnize (Symbol x) = BL.unpack x

getHomeR :: Handler RepPlain
getHomeR = return $ RepPlain $ toContent $ lispnize sexp
  where
    sexp = cons (toSym "list") (toSexp [1::Int, 2, 3])

main :: IO ()
main = warp 3000 HelloWorld
