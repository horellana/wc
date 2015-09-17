module Main where

import Data.Conduit   
import Control.Monad
import Control.Monad.Trans
import Options.Applicative
import qualified Data.Text.Lazy as T
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Combinators as CB

data CmdArguments = CmdArguments { optLines :: Bool, 
                                   optWords :: Bool,
                                   optChars :: Bool,
                                   optVersion :: Bool,
                                   optFiles :: [FilePath] }

version = "0.8.3.0"

cmdArguments :: Parser CmdArguments
cmdArguments = CmdArguments 
               <$> switch (long "lines"
                          <> short 'l'
                          <> help "Count lines in FILES")
               <*> switch (long "words"
                          <> short 'w'
                          <> help "Count words in FILES")
               <*> switch (long "chars"
                          <> short 'm'
                          <> help "Count characters in FILES")
               <*> switch (long "version"
                          <> short 'v'
                          <> help "Show program version and exit")
               <*> many (argument str (metavar "FILES"))

countLines :: (Monad m, Num b) => Conduit () m T.Text -> m b
countLines source = source 
                    $= (CB.linesUnbounded :: Monad m => Conduit T.Text m T.Text)
                    $$ CB.length
                  
-- countChars :: (Monad m, Num b) => Conduit () m T.Text -> m i
countChars source = source $= awaitForever (yield . T.length) $$ CB.sum

countWords :: (Monad m, Num b) => Conduit () m T.Text -> m b
countWords source = source $= CB.linesUnbounded $= wordSource $$ CB.length
    where
      wordSource = awaitForever $ \line -> forM_ (T.words line) yield

wc :: CmdArguments -> IO ()
wc (CmdArguments _ _ _ True _) = putStrLn version
wc (CmdArguments optLines optWords optChars _ optFiles) = runResourceT $ f optFiles
    where
      f [] = doCount CB.stdin >> (lift . putStr) "\n"
      f files = forM_ files $ \file -> doCount (CB.sourceFile file) *> printFile file
      printFile = lift . putStrLn 
      printCount = lift . putStr . (++ " ") . show
      doCount source = do when optLines $ countLines source >>= printCount
                          when optWords $ countWords source >>= printCount
                          when optChars $ countChars source >>= printCount

main :: IO ()
main = execParser parserInfo >>= wc
    where
      parserInfo = info (helper <*> cmdArguments) fullDesc
