module Main where

import qualified Data.Map.Strict as M
import System.Directory
import System.Process
import System.IO
import Data.Char
import Control.Monad

type Checksum = String

catPaths :: FilePath -> FilePath -> FilePath
catPaths a b | last a == '/' || head b == '/' = a++b
             | otherwise                      = a++"/"++b

parceSha256Sum :: String -> [(Checksum,FilePath)]
parceSha256Sum = map foo . lines
  where foo s = let (cksum, rest) = span isHexDigit s
                in id $! (cksum, dropWhile isSpace rest)

checkFiles :: [FilePath] -> IO [(Checksum,FilePath)]
checkFiles [] = return []
checkFiles files = do
  let p = (proc "md5sum" files){ std_out = CreatePipe }
  (_, Just hout, _, _) <- createProcess p
  s <- hGetContents hout
  putStrLn s
  return $ parceSha256Sum s

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
  ms <- mapM checkDir dirs
  return $! M.unionsWith (++) (m:ms)

main :: IO ()
main = do
  m <- checkDir "/mnt/data-i/joshua/Shaffers Photos"
  print m
