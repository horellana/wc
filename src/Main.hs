module Main where

import Data.Conduit
  
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Combinators as CB

import Control.Monad
import Control.Monad.Trans.Resource

import System.Environment (getArgs)
import System.IO (stdin)

countLines :: (MonadResource m) => FilePath -> m Int
countLines file = CB.sourceFile file $=
                  CT.lines $$
                  CB.foldl (\c _ -> c + 1) 0

main :: IO ()
main = do
  files <- getArgs
  fileLines <- runResourceT $ forM files countLines
  print $ sum fileLines
