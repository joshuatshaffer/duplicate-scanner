{-# Language BangPatterns #-}
module Checksum where

import Crypto.Hash
import qualified Data.ByteString.Lazy as B

type Checksum = Digest MD5

checkFile :: FilePath -> IO Checksum
checkFile !file = do
  c <- fmap (hash') $ B.readFile file
  putStrLn $ show c ++ " " ++ file
  return $! c
  where
    -- Yes, I know, MD5... but it's not
    -- like it's for security or anything.
    hash' :: B.ByteString -> Digest MD5
    hash' = hashlazy
