module Main where

import Data.Conduit
  
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Combinators as CB

import Control.Monad.Trans.Resource
import System.IO (stdin)

countLines :: (Num b, MonadResource m) => FilePath -> m b
countLines file = CB.sourceFile file $=
                  CT.lines $$
                  CB.foldl (\c _ -> c + 1) 0

main :: IO ()
main = undefined
