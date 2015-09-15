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
                                   optFiles :: [FilePath] }
                  
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
               <*> many (argument str (metavar "FILES"))

countLines :: (Monad m, Num b) => Conduit () m T.Text -> m b
countLines source = source 
                  $= (CB.linesUnbounded :: Monad m => Conduit T.Text m T.Text)
                  $$ CB.length
                  
countChars :: (Monad m, Num b) => Conduit () m T.Text -> m b
countChars source = source $= CB.linesUnbounded $= charSource $$ CB.length
    where
      charSource = awaitForever $ \line -> forM_ (T.chunksOf 1 line) yield 

countWords :: (Monad m, Num b) => Conduit () m T.Text -> m b
countWords source = source $= CB.linesUnbounded $= wordSource $$ CB.length
    where
      wordSource = awaitForever $ \line -> forM_ (T.words line) yield

wc :: CmdArguments -> IO ()
wc (CmdArguments optLines optWords optChars optFiles) = runResourceT $ f optFiles
    where
      f [] = doCount CB.stdin >> (lift . putStr) "\n"
      f files = forM_ files $ \file -> do doCount $ CB.sourceFile file
                                          printFile file
      printFile = lift . putStrLn 
      printCount = lift . putStr . (++ " ") . show
      doCount source = do when optLines $ countLines source >>= printCount
                          when optWords $ countWords source >>= printCount
                          when optChars $ countChars source >>= printCount

main :: IO ()
main = execParser parserInfo >>= wc
    where
      parserInfo = info (helper <*> cmdArguments) fullDesc
