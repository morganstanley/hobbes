{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( someFunc,
  )
where

import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Lens.Micro.Platform

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
C.include "<issue-398/dawa79.H>"
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

withCc :: (Cc -> IO ()) -> IO ()
withCc fn = new >>= fn

someFunc :: IO ()
someFunc = withCc \_ -> do
  putStrLn "Cc initialized"
  [C.block|void {
          run("/home/smunix/Programming/compilers/hobbes/issue-398.db", true);
          }|]
  putStrLn "Cc ended"
