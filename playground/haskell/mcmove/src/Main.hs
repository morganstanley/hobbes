{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.IO
import Control.Monad
import Control.Arrow
import Data.ByteString as B hiding (putStrLn, zip)
import qualified Data.ByteString.Internal as BI
import Data.Function
import Foreign.C.Types
import Data.Word
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Lens.Micro.Platform
import Text.Disassembler.X86Disassembler

newtype Cc where
  Cc :: {_compiler :: ForeignPtr Cc} -> Cc

makeLenses ''Cc

C.context $
  C.baseCtx
    <> C.bsCtx
    <> C.cppCtx
    <> C.fptrCtx
    <> C.funCtx
    <> C.bsCtx
    <> C.cppTypePairs
      [ ("Cc", [t|Cc|])
      ]

C.include "<hobbes/hobbes.H>"
C.include "<iostream>"

C.using "Cc = hobbes::cc"

class New a where
  new :: IO a

instance New Cc where
  new =
    [C.exp|Cc* {new Cc{}}|]
      >>= \cc ->
        newForeignPtr cc ([C.block|void { delete $(Cc* cc); }|])
          >>= return . Cc

withCC :: (Cc -> IO ()) -> IO ()
withCC fn = new >>= fn

disass :: Cc -> (ByteString -> IO ()) -> ByteString -> IO ()
disass cc fn bstr = do
  [C.block|void {
          auto bytes = $fptr-ptr:(Cc* cc')->machineCodeForExpr(std::string($bs-ptr:bstr, $bs-len:bstr));
          $fun:(void(*bstrFn)(uint8_t const*, size_t))(bytes.data(), bytes.size());
          }|]
  where
    bstrFn :: Ptr Word8 -> CSize -> IO ()
    bstrFn ptr sz = packCStringLen (castPtr ptr, fromIntegral sz) >>= fn
    cc' :: ForeignPtr Cc
    !cc' = cc ^. compiler

main :: IO ()
main = withCC \cc -> do
  ["(\\xs.match xs with | [1,2,3] -> 1 | [1,2,y] -> y | [] -> 9 | _ -> 10) :: [int] -> int", "(\\xs.1) :: [int] -> int", "1", "2", "\"providence\""]
    & traverseOf traverse (\x ->
                             [putStrLn . show, putStrLn . uncurry Prelude.replicate . (B.length &&& const '='), disass cc printASM]
                             <&> ($ x)
                             & sequence)
  return ()
  where
    printASM :: ByteString -> IO ()
    printASM = (fmap (<&> (<&> showAtt)) . disassembleList . B.unpack) >=> (traverseOf (traversed .traversed) putStrLn) >=> (void . return)
