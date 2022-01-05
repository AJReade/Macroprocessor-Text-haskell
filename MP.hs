module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp x table = [ value | (key, value) <- table, key == x]

splitText :: [Char] -> String -> (String, [String])
splitText seps "" = ("", [""])
splitText seps (x : xs)
  | x `elem` seps = (x : seps', "" : text)
  | otherwise           = (seps', (x : y) : ys)
  where
    (seps', text@(y : ys)) = splitText seps xs

combine :: String -> [String] -> [String]
combine "" wordList = wordList
combine (sep : seps) (word : words)
  = word : ([sep] : combine seps words)
combine _ _ = [""]


getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (def : defs) = (y, def') : getKeywordDefs defs
  where
    (x : xs, y : ys) = splitText " " def
    def' = concat (combine xs ys)


expand :: FileContents -> FileContents -> FileContents
expand text info
  = concat [ replaceWord word defs | word <- lstText]
  where
    (seps, preText)  = (splitText separators text)
    lstText = combine seps preText
    (_, keyDefs) = splitText "\n" info
    defs    = getKeywordDefs keyDefs

    -- You may wish to uncomment and implement this helper function
-- when implementing expand
replaceWord :: String -> KeywordDefs -> String
replaceWord word@('$' : tail) keydefs = x
  where
    (x : xs) = (lookUp word keydefs)
replaceWord word _ = word

-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
