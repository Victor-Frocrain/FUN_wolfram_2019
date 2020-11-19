module Lib
    ( showColor
    , favoriteColor
    , combineColor
    , favoriteShowColor
    , function
    , maybeHead
    , fibonacci
    , recursive
    ) where

-- String, Int, Integer, [String], Float,

data Color = RED | YELLOW | GREEN | BLUE | PURPLE
 deriving Show

showColor :: Color -> String
showColor RED = "Super rouge"
showColor BLUE = "C'est bleu"
showColor GREEN = "Vert comme la pelouse"
showColor _ = "J'aime pas cette couleur"

--data Bool = True | False

--data Maybe a = Nothing | Just a

favoriteColor :: Color -> Maybe Color
favoriteColor PURPLE = Just PURPLE
favoriteColor _ = Nothing

combineColor :: Color -> Color -> Maybe Color
combineColor BLUE YELLOW = Just GREEN
combineColor _ _ = Just RED

favoriteShowColor :: Color -> Maybe String
favoriteShowColor color = Just (showColor color)

--List a :: a:[a]

--unElementList = "caro":[]
--deuxElementList = "caro":["gaudreau"]
--listeVide = []

function :: [Color] -> String
function [] = "Rien"
function (RED:[]) = "La tete est rouge"
function (x:xs) = "La tete est " ++ showColor x ++ function xs

--function2 :: [Maybe Color] -> String
--function2 (Nothing:_) = "Rien"
--function2 ((Just RED):_) = function2(YELLOW)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

--type StartingElem = Int
type ElemNb = Int

fibonacci :: ElemNb -> [Int]
fibonacci nb = 1:recursive nb 1 0

type NMinusOne = Int
type NMinusTwo = Int

recursive :: ElemNb -> NMinusOne -> NMinusTwo -> [Int]
recursive 0 _ _ = []
recursive nb nm1 nm2 = tot:(recursive (nb - 1) tot nm1)
    where
        tot = nm1 + nm2

--showColor myColor = "Super couleur! " ++ show myColor

--data Cell = DEAD | ALIVE

--data Step = Step [a] a [a]
--          = Step{
--                left :: [a],
--                center :: a,
--                right :: [a]
--                }

