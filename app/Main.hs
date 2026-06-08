module Main where

import Data.Char (isAlpha, isAlphaNum)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map

import Calculate
import ParseTable

type Variables = Map.Map String Double
type Functions = Map.Map String (String, String)
-- function name -> (parameter name, function body)

main :: IO ()
main = loop Map.empty Map.empty

loop :: Variables -> Functions -> IO ()
loop vars funcs = do
  input <- prompt "> "

  if input == "quit"
    then putStrLn "Goodbye!"
    else
      case parseFunctionDef input of

        -- Handles input like: f(x) = x * 2
        Just (fname, param, body)
          | validName fname && validName param -> do
              let newFuncs = Map.insert fname (param, body) funcs
              putStrLn ("Stored function " ++ fname)
              loop vars newFuncs

          | otherwise -> do
              putStrLn "Invalid function definition."
              loop vars funcs

        Nothing ->
          case splitAssignment input of

            -- Handles input like: x = 5
            Just (name, rhs)
              | validName name ->
                  case replaceVars vars funcs rhs of
                    Left err -> do
                      putStrLn err
                      loop vars funcs
                    Right expression ->
                      case parseTable fastExpr expression of
                        [] -> do
                          putStrLn ("Invalid assignment: " ++ input)
                          loop vars funcs
                        (n, _):_ -> do
                          let newVars = Map.insert name n vars
                          putStrLn ("Stored " ++ name ++ " = " ++ show n)
                          loop newVars funcs

              | otherwise -> do
                  putStrLn "Invalid variable name."
                  loop vars funcs

            -- Handles normal calculator expressions
            Nothing ->
              case replaceVars vars funcs input of
                Left err -> do
                  putStrLn err
                  loop vars funcs
                Right expression ->
                  case parseTable fastExpr expression of
                    [] -> do
                      putStrLn ("Not an arithmetic expression: " ++ input)
                      loop vars funcs
                    (n, _):_ -> do
                      if isInfinite n || isNaN n
                        then putStrLn "Math error."
                        else print n
                      loop vars funcs

splitAssignment :: String -> Maybe (String, String)
splitAssignment input =
  case break (== '=') input of
    (left, '=':right) -> Just (trimString left, trimString right)
    _                -> Nothing

parseFunctionDef :: String -> Maybe (String, String, String)
parseFunctionDef input =
  case break (== '=') input of
    (left, '=':right) ->
      case span (/= '(') left of
        (fname, '(':rest) ->
          case span (/= ')') rest of
            (param, ')':_) ->
              Just (trimString fname, trimString param, trimString right)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

replaceVars :: Variables -> Functions -> String -> Either String String
replaceVars vars funcs [] = Right []
replaceVars vars funcs (c:cs)
  | isAlpha c =
      let (nameRest, rest) = span isAlphaNum cs
          name = c : nameRest
      in
        if name `elem` ["sqrt", "abs", "sin", "cos"]
          then do
            remaining <- replaceVars vars funcs rest
            Right (name ++ remaining)
          else if name == "pi"
            then do
            remaining <- replaceVars vars funcs rest
            Right (show pi ++ remaining)

          else case Map.lookup name funcs of
            Just (param, body) ->
              case rest of
                '(' : afterOpen ->
                  case break (== ')') afterOpen of
                    (arg, ')' : afterClose) -> do
                      let expanded = replaceParam param ("(" ++ arg ++ ")") body
                      remaining <- replaceVars vars funcs afterClose
                      finalExpanded <- replaceVars vars funcs expanded
                      Right (finalExpanded ++ remaining)
                    _ ->
                      Left ("Invalid function call: " ++ name)
                _ ->
                  Left ("Invalid function call: " ++ name)

            Nothing ->
              case Map.lookup name vars of
                Just value -> do
                  remaining <- replaceVars vars funcs rest
                  Right (show value ++ remaining)
                Nothing ->
                  Left ("Error: variable '" ++ name ++ "' is not defined.")

  | otherwise = do
      remaining <- replaceVars vars funcs cs
      Right (c : remaining)

replaceParam :: String -> String -> String -> String
replaceParam param value [] = []
replaceParam param value (c:cs)
  | isAlpha c =
      let (nameRest, rest) = span isAlphaNum cs
          name = c : nameRest
      in
        if name == param
          then value ++ replaceParam param value rest
          else name ++ replaceParam param value rest
  | otherwise =
      c : replaceParam param value cs

validName :: String -> Bool
validName [] = False
validName (c:cs) = isAlpha c && all isAlphaNum cs

trimString :: String -> String
trimString = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

prompt :: String -> IO String
prompt lead = do
  putStr lead
  hFlush stdout
  getLine