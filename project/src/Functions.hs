module Functions where

data Rule = Rule30 | Rule90 | Rule110 | RuleU

data Params = Params { rule :: Rule
                     , width :: Int
                     , line :: Int
                     , start :: Int
                     , move :: Int
                     , err :: Bool
                     , lineDefined :: Bool
                     }

checkParams :: Params -> Int -> String
checkParams (Params{rule=RuleU}) _ = "Undefined"
checkParams (Params{rule=Rule30, width=width, line=line, start=start, move=move, lineDefined=defined}) nb = if ((line > 0 || defined == False) && start >= 0) then makeRule30 "" nb width (line + start - 1) start move
                                                                                                 else ""
checkParams (Params{rule=Rule90, width=width, line=line, start=start, move=move, lineDefined=defined}) nb = if ((line > 0 || defined == False) && start >= 0) then makeRule90 "" nb width (line + start - 1) start move
                                                                                                 else ""
checkParams (Params{rule=Rule110, width=width, line=line, start=start, move=move, lineDefined=defined}) nb = if ((line > 0 || defined == False) && start >= 0) then makeRule110 "" nb width (line + start - 1) start move
                                                                                                  else ""

makeRule30 :: String -> Int -> Int -> Int -> Int -> Int -> String
makeRule30 prevGen nb width lines start move = if (nb == lines && start == 0) then makeLine newGen nb width lines move
                                          else if (start == 0) then (makeLine newGen nb width lines move) ++ (makeRule30 newGen (nb + 1) width lines start move)
                                          else (makeRule30 newGen (nb + 1) width lines (start - 1) move)
                                            where
                                                newGen = (computeRule30 "" (nb * 2 + 1) width 0 True prevGen)
                        
computeRule30 :: String -> Int -> Int -> Int -> Bool -> String -> String
computeRule30 s 0 width i _ _ = s
computeRule30 s nb 0 i _ _ = s ++ "*"
computeRule30 s nb width 0 True prevGen = computeRule30 (s ++ "*") (nb - 1) (width) 0 False prevGen
computeRule30 s nb width i False prevGen = if ((i + 1) < (length prevGen) && i > 0) then computeRule30 (s ++ (computeCase30 (prevGen!!(i - 1)) (prevGen!!i) (prevGen!!(i + 1)))) (nb - 1) (width) (i + 1) False prevGen
                             else if (i < (length prevGen) && i > 0) then computeRule30 (s ++ (computeCase30 (prevGen!!(i - 1)) (prevGen!!i) ' ')) (nb - 1) (width) (i + 1) False prevGen
                             else if (i < (length prevGen) && i == 0) then computeRule30 (s ++ (computeCase30 ' ' (prevGen!!i) ' ')) (nb - 1) (width) (i + 1) False prevGen
                             else s ++ "*"

computeCase30 :: Char -> Char -> Char -> String
computeCase30 '*' '*' '*' = " "
computeCase30 '*' '*' ' ' = " "
computeCase30 '*' ' ' '*' = " "
computeCase30 '*' ' ' ' ' = "*"
computeCase30 ' ' '*' '*' = "*"
computeCase30 ' ' '*' ' ' = "*"
computeCase30 ' ' ' ' '*' = "*"
computeCase30 ' ' ' ' ' ' = " "

makeRule90 :: String -> Int -> Int -> Int -> Int -> Int -> String
makeRule90 prevGen nb width lines start move = if (nb == lines && start == 0) then makeLine newGen nb width lines move
                                          else if (start == 0) then (makeLine newGen nb width lines move) ++ (makeRule90 newGen (nb + 1) width lines start move)
                                          else (makeRule90 newGen (nb + 1) width lines (start - 1) move)
                                            where
                                                newGen = (computeRule90 "" (nb * 2 + 1) width 0 True prevGen)
                        
computeRule90 :: String -> Int -> Int -> Int -> Bool -> String -> String
computeRule90 s 0 width i _ _ = s
computeRule90 s nb 0 i _ _ = s ++ "*"
computeRule90 s nb width 0 True prevGen = computeRule90 (s ++ "*") (nb - 1) (width) 0 False prevGen
computeRule90 s nb width i False prevGen = if ((i + 1) < (length prevGen) && i > 0) then computeRule90 (s ++ (computeCase90 (prevGen!!(i - 1)) (prevGen!!i) (prevGen!!(i + 1)))) (nb - 1) (width) (i + 1) False prevGen
                             else if (i < (length prevGen) && i > 0) then computeRule90 (s ++ (computeCase90 (prevGen!!(i - 1)) (prevGen!!i) ' ')) (nb - 1) (width) (i + 1) False prevGen
                             else if (i < (length prevGen) && i == 0) then computeRule90 (s ++ (computeCase90 ' ' (prevGen!!i) ' ')) (nb - 1) (width) (i + 1) False prevGen
                             else s ++ "*"

computeCase90 :: Char -> Char -> Char -> String
computeCase90 '*' '*' '*' = " "
computeCase90 '*' '*' ' ' = "*"
computeCase90 '*' ' ' '*' = " "
computeCase90 '*' ' ' ' ' = "*"
computeCase90 ' ' '*' '*' = "*"
computeCase90 ' ' '*' ' ' = " "
computeCase90 ' ' ' ' '*' = "*"
computeCase90 ' ' ' ' ' ' = " "

makeRule110 :: String -> Int -> Int -> Int -> Int -> Int -> String
makeRule110 prevGen nb width lines start move = if (nb == lines && start == 0) then makeLine newGen nb width lines move
                                          else if (start == 0) then (makeLine newGen nb width lines move) ++ (makeRule110 newGen (nb + 1) width lines start move)
                                          else (makeRule110 newGen (nb + 1) width lines (start - 1) move)
                                            where
                                                newGen = (computeRule110 "" (nb * 2 + 1) width 0 True prevGen)
                        
computeRule110 :: String -> Int -> Int -> Int -> Bool -> String -> String
computeRule110 s 0 width i _ _ = s
computeRule110 s nb 0 i _ _ = s ++ "*"
computeRule110 s nb width 0 True prevGen = computeRule110 (s ++ "*") (nb - 1) (width) 0 False prevGen
computeRule110 s nb width i False prevGen = if ((i + 1) < (length prevGen) && i > 0) then computeRule110 (s ++ (computeCase110 (prevGen!!(i - 1)) (prevGen!!i) (prevGen!!(i + 1)))) (nb - 1) (width) (i + 1) False prevGen
                             else if (i < (length prevGen) && i > 0) then computeRule110 (s ++ (computeCase110 (prevGen!!(i - 1)) (prevGen!!i) ' ')) (nb - 1) (width) (i + 1) False prevGen
                             else if (i < (length prevGen) && i == 0) then computeRule110 (s ++ (computeCase110 ' ' (prevGen!!i) ' ')) (nb - 1) (width) (i + 1) False prevGen
                             else s ++ " "

computeCase110 :: Char -> Char -> Char -> String
computeCase110 '*' '*' '*' = " "
computeCase110 '*' '*' ' ' = "*"
computeCase110 '*' ' ' '*' = "*"
computeCase110 '*' ' ' ' ' = " "
computeCase110 ' ' '*' '*' = "*"
computeCase110 ' ' '*' ' ' = "*"
computeCase110 ' ' ' ' '*' = "*"
computeCase110 ' ' ' ' ' ' = " "

makeLine :: String -> Int -> Int -> Int -> Int -> String
makeLine s nb width lines move = if (len > width && nb == lines) then (makeSpace ("") firstLen) ++ (cutStr s (len - width) (newMove)) ++ (makeSpace ("") secondLen)
                            else if (len > width) then (makeSpace ("") firstLen) ++ (cutStr s (len - width) (newMove)) ++ (makeSpace ("") secondLen) ++ "\n"
                            else if (firstLen <= 0 && nb == lines) then (cutStrBegin s (secondLen + len) width) ++ (makeSpace ("") secondLen)
                            else if (firstLen <= 0) then (cutStrBegin s (secondLen + len) width) ++ (makeSpace ("") secondLen) ++ "\n"
                            else if (secondLen <= 0 && nb == lines) then (makeSpace ("") firstLen) ++ (cutStrEnd s (firstLen + len) width)
                            else if (secondLen <= 0) then (makeSpace ("") firstLen) ++ (cutStrEnd s (firstLen + len) width) ++ "\n"
                            else if (len <= width && nb == lines) then ((makeSpace ("") firstLen) ++ s ++ (makeSpace ("") secondLen))
                            else ((makeSpace ("") firstLen) ++ s ++ (makeSpace ("") secondLen) ++ "\n")
                                where
                                  len = length s
                                  firstLen = (div width 2) - (div len 2) + move
                                  beginLen = if (firstLen < 0) then 0
                                             else firstLen
                                  secondLen = if (mod width 2 /= 0) then (div width 2) - (div len 2) - move
                                              else (div width 2) - (len - (div len 2)) - move
                                  endLen = if (secondLen < 0) then 0
                                           else secondLen
                                  newMove = if (move < 0 && (mod width 2) /= 0) then move - (2 - endLen)
                                            else if (move > 0 && (mod width 2) /= 0) then move + (2 - beginLen)
                                            else if (move < 0) then move - (1 - endLen)
                                            else if (move > 0) then move + (3 - beginLen)
                                            else move

cutStr :: String -> Int -> Int -> String
cutStr s i move = if (move < 0) then cutStr (tail s) (i - 1) (move + 1)
                  else if (move > 0) then cutStr (init s) (i - 1) (move - 1)
                  else if (i == (1)) then init s
                  else if (i > 1) then cutStr ((init . tail) s) (i - 2) move
                  else s

cutStrBegin :: String -> Int -> Int -> String
cutStrBegin s i width = if (i > width) then cutStrBegin (tail s) (i - 1) width
                        else s

cutStrEnd :: String -> Int -> Int -> String
cutStrEnd s i width = if (i > width) then cutStrEnd (init s) (i - 1) width
                      else s

charToStr :: Char -> String
charToStr c = [c]

makeSpace :: String -> Int -> String
makeSpace s nb = if (nb > 0) then makeSpace (s ++ " ") (nb - 1)
                 else s