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

wc :: CmdArguments -> IO ()
wc (CmdArguments optLines optFiles) =
    runResourceT $ forM_ optFiles $ 
                     \file -> when optLines $ do countLines file >>= printCount
                                                 printFile file
    where
      printCount = lift . putStr . (++ " ") . show
      printFile = lift . putStrLn 

main :: IO ()
main = execParser parserInfo >>= wc
    where
      parserInfo = info (helper <*> cmdArguments) fullDesc
