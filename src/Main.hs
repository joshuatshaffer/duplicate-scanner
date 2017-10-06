module Main where

import Checksum
import Parallel

import qualified Data.Map.Strict as M
import System.Directory
import Data.List (intercalate)
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent

catPaths :: FilePath -> FilePath -> FilePath
catPaths a b | doob && boob = a ++ tail b
             | doob || boob = a ++ b
             | otherwise    = a ++ "/" ++ b
             where doob = last a == '/'
                   boob = head b == '/'

newtype CheckRez = CheckRez (M.Map Checksum [FilePath])

instance Monoid CheckRez where
    mempty = CheckRez M.empty
    mappend (CheckRez x) (CheckRez y) = CheckRez $ M.unionWith (++) x y

check :: FilePath -> IO CheckRez
check path = do
    ex <- doesPathExist path
    sim <- pathIsSymbolicLink path
    file <- doesFileExist path
    if ex && not sim
        then if file
            then fmap (\c -> CheckRez $ M.singleton c [path]) $ checkFile path
            else listDirectory path >>= (mapMPar check . map (catPaths path))
        else mempty

prettyPrintMap :: (Show k, Show a) => M.Map k [a] -> String
prettyPrintMap = unlines . map foo . M.assocs
  where
    foo :: (Show k, Show a) => (k,[a]) -> String
    foo (k,a) = intercalate "\n  " $ show k : map show a

main :: IO ()
main = do
  (CheckRez m) <- check "/mnt/data-i/joshua/Shaffers Photos"
  print $ M.foldr (\a len -> len + (length a)) 0 m
  putStrLn . prettyPrintMap $ M.filter ((>1) . length) m -- M.filter ((>1) . length)
