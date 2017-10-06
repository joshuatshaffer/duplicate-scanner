module Main (main) where

import Checksum
import Parallel

import qualified Data.Map.Strict as M
import System.Directory
import Data.List (intercalate)
import Control.Monad ((>=>),foldM,filterM)
import System.Environment (getArgs)

catPaths :: FilePath -> FilePath -> FilePath
catPaths a b | doob && boob = a ++ tail b
             | doob || boob = a ++ b
             | otherwise    = a ++ "/" ++ b
             where doob = last a == '/'
                   boob = head b == '/'

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p xs = foldM (selectM p) ([],[]) $ reverse xs

selectM :: Monad m => (a -> m Bool) -> ([a], [a]) -> a -> m ([a], [a])
selectM p ~(ts,fs) x = do
    b <- p x
    return $ if b then (x:ts, fs)
                  else (ts, x:fs)

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' p = listDirectory p >>= (return . map (catPaths p))

listFiles :: FilePath -> IO [FilePath]
listFiles = listDirectory' >=>
            filterM (fmap not . pathIsSymbolicLink) >=>
            partitionM doesFileExist >=> foo
  where
    foo (fs, ds) = do fss <- mapM listFiles ds
                      return $ concat (fs:fss)

newtype CheckRez = CheckRez (M.Map Checksum [FilePath])

instance Monoid CheckRez where
    mempty = CheckRez M.empty
    mappend (CheckRez x) (CheckRez y) = CheckRez $ M.unionWith (++) x y

check :: Int -> FilePath -> IO CheckRez
check n = listFiles >=> nthreads n checkFile'
  where checkFile' p = fmap (\c -> CheckRez $ M.singleton c [p]) $ checkFile p

prettyPrintMap :: (Show k, Show a) => M.Map k [a] -> String
prettyPrintMap = unlines . map foo . M.assocs
  where
    foo :: (Show k, Show a) => (k,[a]) -> String
    foo (k,a) = intercalate "\n  " $ show k : map show a

main :: IO ()
main = do
  args <- getArgs
  let n = read $ head args
  let p = args !! 1
  (CheckRez m) <- check n p
  print $ M.foldr (\a len -> len + (length a)) 0 m
  putStrLn . prettyPrintMap $ M.filter ((>1) . length) m
