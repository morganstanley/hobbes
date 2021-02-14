{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.IO
import Control.Monad
import Data.ByteString as B hiding (putStrLn, zip)
import qualified Data.ByteString.Internal as BI
import Data.Function
import Foreign.C.Types
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Lens.Micro.Platform

newtype Cc where
  Cc :: {_compiler :: ForeignPtr Cc} -> Cc

makeLenses ''Cc

newtype CharLen where
  CharLen :: {_charLen :: ForeignPtr CharLen} -> CharLen

makeLenses ''CharLen

C.context $
  C.baseCtx
    <> C.bsCtx
    <> C.cppCtx
    <> C.fptrCtx
    <> C.funCtx
    <> C.bsCtx
    <> C.cppTypePairs
      [ ("Cc", [t|Cc|]),
        ("CharLen", [t|CharLen|])
      ]

C.include "<hobbes/hobbes.H>"
C.include "<iostream>"

C.using "Cc = hobbes::cc"
C.using "CharLen = std::pair<unsigned char*, size_t>"

class New a where
  new :: IO a

instance New Cc where
  new =
    [C.exp|Cc* {new Cc{}}|]
      >>= \cc ->
        newForeignPtr cc ([C.block|void { delete $(Cc* cc); }|])
          >>= return . Cc

class Convert b m a where
  convert :: a -> m b

instance Convert ByteString IO CharLen where
  convert cl = do
    c <- [C.exp|unsigned char*{ $fptr-ptr:(CharLen *cl')->first }|]
    l <- [C.exp|size_t{ $fptr-ptr:(CharLen *cl')->second }|]
    packCStringLen (castPtr c, fromIntegral l)
    where
      !cl' = cl ^. charLen

disassemble :: Cc -> ByteString -> IO ByteString
disassemble cc = getMachineCode >=> convert
  where
    getMachineCode :: ByteString -> IO CharLen
    getMachineCode bstr = do
      cl <- [C.block|CharLen*{
              auto bytes = $fptr-ptr:(Cc* cc')->machineCodeForExpr(std::string($bs-ptr:bstr, $bs-len:bstr));
              auto r = new CharLen(new uint8_t[bytes.size()], bytes.size());
              std::memcpy(r->first, bytes.data(), bytes.size());
              std::cout << "disassemble:" << std::string(reinterpret_cast<char*>(bytes.data()), bytes.size()) << std::endl;
              std::cout << "size:disassemble:" << bytes.size() << std::endl;
              return r;
              }|]
      newForeignPtr cl ([C.block|void{ delete [] $(CharLen *cl)->first; delete $(CharLen *cl);}|])
        >>= return . CharLen
      where
        !cc' = cc ^. compiler

main :: IO ()
main = do
  (cc :: Cc) <- new
  disassemble cc "(\\xs.match xs with | [1,2,3] -> 1 | [1,2,y] -> y | [] -> 9 | _ -> 10) :: [int] -> int"
        >>= putStrLn . show . zip [1..] . fmap BI.w2c . B.unpack
  x <-
    [C.block| int{
                 auto c = [&]()->hobbes::cc& {
                   thread_local hobbes::cc cc;
                   return cc;
                 };
                 auto disassemble = [&](std::string const& expr, unsigned char*& p, size_t& len) -> void {
                   auto bytes = c().machineCodeForExpr(expr);
                   p = bytes.data();
                   len = bytes.size();
                 };
                 auto x = new int{};
                 auto y = c().compileFn<int()>("{a=1,b=2,c=3}.b")();
                 printf("Some number: %.2f\n", cos(0.5));
                 return (uint64_t)y;
               } |]
  putStrLn $ show x ++ " characters printed."
