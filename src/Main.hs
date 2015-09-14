module Main where
    
import Data.Conduit

import Options.Applicative
  
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Combinators as CB

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
    
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
        
wc :: MonadResource m => CmdArguments -> ConduitM i String m ()
wc (CmdArguments True files) = 
    forM_ files $ \file -> do count <- countLines file
                              yield $ show count ++ " " ++ file

main :: IO ()
main = do args <- execParser parserInfo 
          runResourceT $ wc args $$ CB.mapM_ $ lift . putStrLn
    where
      parserInfo = info (helper <*> cmdArguments) fullDesc
