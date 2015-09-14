module Main where
    
import Data.Conduit

import Options.Applicative
  
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Combinators as CB

import Control.Monad
import Control.Monad.Trans.Resource
    
import Data.List (intercalate)
    
data CmdArguments = CmdArguments { optLines :: Bool, 
                                   optFiles :: [FilePath] }
                  deriving (Show)

cmdArguments :: Parser CmdArguments
cmdArguments = CmdArguments 
               <$> switch (long "lines"
                          <> help "Count lines in FILES")
               <*> many (argument str (metavar "FILES"))
                   
countLines :: (MonadResource m) => FilePath -> m Int
countLines file = CB.sourceFile file $= CT.lines $$ CB.length
        
wc :: CmdArguments -> IO String
wc (CmdArguments True files) = do
  results <- runResourceT $ forM files $ 
       \file -> do count <- countLines file
                   return $ show count ++ " " ++ file
  return $ intercalate "\n" results
                          
main :: IO ()
main = execParser parserInfo >>= wc >>= putStrLn
    where
      parserInfo = info (helper <*> cmdArguments) fullDesc
                   
