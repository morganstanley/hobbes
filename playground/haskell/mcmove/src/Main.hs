{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

C.context $
  C.baseCtx
    <> C.bsCtx
    <> C.cppCtx
    <> C.fptrCtx
    <> C.funCtx

-- C.include "<stdio.h>"
-- C.include "<math.h>"
C.include "<hobbes/hobbes.H>"

main :: IO ()
main = do
  -- x <- [C.exp| int{ printf("Some number: %.2f\n", cos(0.5)) } |]
  putStrLn $ show 10 ++ " characters printed."
