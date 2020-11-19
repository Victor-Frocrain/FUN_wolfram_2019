module Main where

import Functions
import System.Exit
import System.Environment
import Text.Read

main :: IO ()
main = do
  args <- getArgs
  let params = handleArgs args defaultParams
  let str = checkParams params 0
  if (str == "Undefined") then exitWith (ExitFailure 84)
  else if (checkIfError params) then exitWith (ExitFailure 84)
  else if (checkIfNegative params) then exitWith (ExitFailure 84)
  else if (str == "") then exitWith (ExitSuccess)
  else putStrLn str
  exitWith (ExitSuccess)


--datatype
defaultRule = RuleU
defaultWidth = 80
defaultLines = -1

defaultParams = Params {rule = defaultRule, width = defaultWidth, line = defaultLines, start = 0, move = 0, err = False, lineDefined = False}

checkIfError :: Params -> Bool
checkIfError (Params{err=True}) = True
checkIfError (Params{err=False}) = False

checkIfNegative :: Params -> Bool
checkIfNegative (Params{width=width, line=line, start=start, move=move, lineDefined=defined}) = if (width < 0) then True
                                                                                                else if (line < 0 && defined) then True
                                                                                                else if (move < ((div width 2) * (-1)) || move > (div width 2)) then True
                                                                                                else if (start < 0) then True
                                                                                                else False

handleArgs :: [String] -> Params -> Params
handleArgs [] params = params
handleArgs (_:[]) params = params {rule = RuleU}
handleArgs ("--rule":rNb:args) params = handleArgs args (params {rule = handleRuleNb rNb})
handleArgs ("--window":rNb:args) params = handleArgs args (params {width = handleSize rNb, err = handleErrorInt rNb})
handleArgs ("--lines":rNb:args) params = handleArgs args (params {line = handleSize rNb, err = handleErrorInt rNb, lineDefined = True})
handleArgs ("--start":rNb:args) params = handleArgs args (params {start = handleSize rNb, err = handleErrorInt rNb})
handleArgs ("--move":rNb:args) params = handleArgs args (params {move = handleSize rNb, err = handleErrorInt rNb})
handleArgs (_:args) params = handleArgs args params

handleRuleNb :: String -> Rule
handleRuleNb s = case (readMaybe s :: Maybe Int) of
                   Nothing -> defaultRule
                   Just 30 -> Rule30
                   Just 90 -> Rule90
                   Just 110 -> Rule110
                   Just _ -> defaultRule

handleSize :: String -> Int
handleSize s = case (readMaybe s :: Maybe Int) of
                Nothing -> (-1)
                Just a -> a

handleErrorInt :: String -> Bool
handleErrorInt s = case (readMaybe s :: Maybe Int) of
                    Nothing -> True
                    Just a -> False