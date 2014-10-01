-- | A cache system for objects with a structured "build" operation.
-- 
-- Every object has a recipe containing its dependencies and the operation.
module Cache ( CObject(..)
             , coHash
             , cached
             , isCached
             ) where

import Control.Monad
import Crypto.Hash.SHA1
import Data.Char
import Data.List
import Data.String
import System.Directory
import Text.Printf

import qualified Data.ByteString as B

-- | A 'CObject' is like a key.
-- It has dependencies and a "recipe" for building it.
data CObject =
    CObject { coOp :: String
            , coDeps :: [B.ByteString]
            , coBuild :: FilePath -> IO ()
            }

instance Show CObject where
    show (CObject { coOp = op, coDeps = deps }) =
        concat [ "CObject { coOp = "
               , show op
               , ", coDeps = "
               , show deps
               , ", _ }"
               ]

-- | Turn a 'CObject' to a 'ByteString' to express it as a dependency.
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

-- | Build or retrieve an object associated to a 'CObject'.
cached :: CObject -> IO FilePath
cached co = do
    let path = coFile co
    ex <- doesFileExist path
    unless ex $ coBuild co path
    return path

-- | Find out if the object already exists.
-- No guarantees as race conditions can occur.
isCached :: CObject -> IO Bool
isCached = doesFileExist . coFile
