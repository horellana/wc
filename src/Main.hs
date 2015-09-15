module Main where
    
import Data.Conduit
import qualified Data.Text as T
    
import Options.Applicative
  
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Combinators as CB

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
    
data CmdArguments = CmdArguments { optLines :: Bool, 
                                   optWords :: Bool,
                                   optFiles :: [FilePath] }
                  deriving (Show)

cmdArguments :: Parser CmdArguments
cmdArguments = CmdArguments 
               <$> switch (long "lines"
                          <> short 'l'
                          <> help "Count lines in FILES")
               <*> switch (long "words"
                          <> short 'w'
                          <> help "Count words in FILES")
               <*> many (argument str (metavar "FILES"))
                   
countLines :: (MonadResource m) => FilePath -> m Int
countLines file = CB.sourceFile file $= CT.lines $$ CB.length
                  
countWords :: (MonadResource m) => FilePath -> m Int
countWords file = CB.sourceFile file $= CT.lines $= wordsSource $$ CB.length
    where
      wordsSource = do line <- await
                       case line of 
                         Just line -> forM_ (T.words line) yield >> wordsSource
                         Nothing -> return ()

wc :: CmdArguments -> IO ()
wc (CmdArguments optLines optWords optFiles) =
    runResourceT $ forM_ optFiles $ 
                     \file -> do when optLines $ countLines file >>= printCount
                                 when optWords $ countWords file >>= printCount
                                 printFile file
    where
      printCount = lift . putStr . (++ " ") . show
      printFile = lift . putStrLn 

main :: IO ()
main = execParser parserInfo >>= wc
    where
      parserInfo = info (helper <*> cmdArguments) fullDesc
