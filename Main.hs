module Main where

import qualified Data.Map.Strict as M
import System.Directory
import System.Process
import System.IO
import Data.Char
import Data.List (intercalate)
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent

type Checksum = String

catPaths :: FilePath -> FilePath -> FilePath
catPaths a b | last a == '/' || head b == '/' = a++b
             | otherwise                      = a++"/"++b

mapMpar :: (a -> IO b) -> [a] -> IO [b]
mapMpar f xs = do
  mv <- newMVar []
  sem <- newQSemN 0
  mapM_ (\x -> forkFinally (f x) (collectResult sem mv)) xs
  waitQSemN sem (length xs)
  takeMVar mv
  where
    collectResult :: QSemN -> MVar [a] -> Either b a -> IO ()
    collectResult sem mv (Right y) =
      do ys <- takeMVar mv
         putMVar mv (y:ys)
         signalQSemN sem 1
    collectResult sem _  (Left _) = signalQSemN sem 1

checkFile :: FilePath -> IO Checksum
checkFile file = do
  let p = (proc "md5sum" [file]){ std_out = CreatePipe }
  (_, Just hout, _, _) <- createProcess p
  s <- hGetContents hout
  putStrLn . head $ lines s
  return $! takeWhile isHexDigit s

checkFiles :: [FilePath] -> IO [(Checksum,FilePath)]
checkFiles = mapMpar (\f -> checkFile f >>= \c -> return (c, f))

partFilesAndDirs :: FilePath -> [FilePath] -> IO ([FilePath], [FilePath])
partFilesAndDirs wd = preFilter >=> part
  where
    preFilter :: [FilePath] -> IO [FilePath]
    preFilter = filterM doesPathExist . map (catPaths wd) >=>
                filterM (fmap not . pathIsSymbolicLink)
    part :: [FilePath] -> IO ([FilePath], [FilePath])
    part zs = do
      dirs <- filterM doesDirectoryExist zs
      files <- filterM doesFileExist zs
      return (dirs,files)

checkDir :: FilePath -> IO (M.Map Checksum [FilePath])
checkDir dir = do
  putStrLn $ "Checking dir " ++ dir
  (dirs,files) <- listDirectory dir >>= partFilesAndDirs dir
  bar <- checkFiles files
  let m = M.fromListWith (++) $ map (\(c,p) -> (c,[p])) bar
  ms <- mapMpar checkDir dirs
  return $! M.unionsWith (++) (m:ms)

prettyPrintMap :: (Show k, Show a) => M.Map k [a] -> String
prettyPrintMap = unlines . map foo . M.assocs
  where
    foo :: (Show k, Show a) => (k,[a]) -> String
    foo (k,a) = intercalate "\n  " $ show k : map show a

main :: IO ()
main = do
  m <- checkDir "/mnt/data-i/joshua/Shaffers Photos"
  putStrLn . prettyPrintMap $ M.filter ((>1) . length) m
