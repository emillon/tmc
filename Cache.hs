-- | A cache system for objects with a structured "build" operation.
-- 
-- Every object has a recipe containing its dependencies and the operation.
module Cache ( CObject(..)
             , coHash
             , cached
             ) where

import Control.Monad
import Crypto.Hash.SHA1
import Data.Char
import Data.List
import Data.String
import System.Directory
import Text.Printf

import qualified Data.ByteString as B

data CObject =
    CObject { coOp :: String
            , coDeps :: [B.ByteString]
            }
    deriving (Show)

coHash :: CObject -> B.ByteString
coHash co =
    toHex $ hash $ B.intercalate newline $ header : sort (coDeps co)
        where
            header = fromString $ coOp co
            newline = B.singleton 0x0a

toHex :: B.ByteString -> B.ByteString
toHex bytes = fromString $ concatMap (printf "%02x") (B.unpack bytes)

coFile :: CObject -> FilePath
coFile co = "/tmp/" ++ byteStringToString (coHash co) ++ ".wav"

byteStringToString :: B.ByteString -> String
byteStringToString = map (chr . fromIntegral) . B.unpack

cached :: CObject -> (FilePath -> IO ()) -> IO FilePath
cached co act = do
    let path = coFile co
    ex <- doesFileExist path
    when (not ex) $ act path
    return path