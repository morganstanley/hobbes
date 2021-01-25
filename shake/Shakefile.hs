{-# LANGUAGE BlockArguments #-}

import Control.Applicative
import Control.Arrow
import qualified Control.Concurrent as Conc
import Data.Char (toLower)
import Data.List
import Data.List (intercalate, isPrefixOf)
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Language.C
import Development.Shake.Language.C
  ( BuildFlags,
    compilerFlags,
    libraries,
    libraryPath,
    linkerFlags,
    systemIncludes,
    userIncludes,
  )
import Development.Shake.Language.C.Label (append)
import qualified Development.Shake.Language.C.PkgConfig as PkgConfig
import Development.Shake.Language.C.Target.OSX
import qualified Development.Shake.Language.C.Target.OSX as OSX
-- import Development.Shake.Language.C.Util (words')
import Development.Shake.Util

-- credits: copied below hidden library functions from Development.Shake.Language.C.Util
mapFlag :: String -> [String] -> [String]
mapFlag f = concatMap (\x -> [f, x])

concatMapFlag :: String -> [String] -> [String]
concatMapFlag f = map (f ++)

-- | Escape spaces with '\\' character.
--
-- >>> escapeSpaces "string contains spaces"
-- "string\\ contains\\ spaces"
--
-- >>> escapeSpaces " leading and trailing spaces "
-- "\\ leading\\ and\\ trailing\\ spaces\\ "
--
-- >>> escapeSpaces "noSpaces"
-- "noSpaces"
escapeSpaces :: String -> String
escapeSpaces [] = []
escapeSpaces (' ' : xs) = '\\' : ' ' : escapeSpaces xs
escapeSpaces ('\\' : xs) = '\\' : '\\' : escapeSpaces xs
escapeSpaces (x : xs) = x : escapeSpaces xs

-- | Split a list of space separated strings.
--
-- Spaces can be escaped by '\\'.
--
-- >>> words' "word and word\\ with\\ spaces"
-- ["word","and","word with spaces"]
words' :: String -> [String]
words' = unescape . words
  where
    escape = "\\"
    escapeLength = length escape
    isEscaped = isSuffixOf escape
    dropEscape = (++ " ") . reverse . drop escapeLength . reverse
    unescape [] = []
    unescape [x] = [if isEscaped x then dropEscape x else x]
    unescape (x1 : x2 : xs)
      | isEscaped x1 = unescape ((dropEscape x1 ++ x2) : xs)
      | otherwise = [x1] ++ unescape (x2 : xs)

-- ====================================================================
-- LLVMConfig

-- TODO:
--  * Use parsec or attoparsec for more robust parser
--  * Parse preprocessor defines
--  * Parse framework path (-F) and -framework flags

parseCflags :: [String] -> (BuildFlags -> BuildFlags)
parseCflags [] = id
parseCflags (x : xs)
  | isPrefixOf "-I" x = parseCflags xs . append systemIncludes [drop 2 x]
  | isPrefixOf "-i" x = parseCflags xs . append userIncludes [drop 2 x]
  | otherwise = append compilerFlags [(Nothing, [x])]

parseCxxflags :: [String] -> (BuildFlags -> BuildFlags)
parseCxxflags [] = id
parseCxxflags (x : xs)
  | isPrefixOf "-I" x = parseCxxflags xs . append systemIncludes [drop 2 x]
  | isPrefixOf "-i" x = parseCxxflags xs . append userIncludes [drop 2 x]
  | isPrefixOf "-D" x = parseCxxflags xs . append defines [(drop 2 x, Nothing)]
  | otherwise = append compilerFlags [(Nothing, [x])]

parseLibs :: [String] -> (BuildFlags -> BuildFlags)
parseLibs [] = id
parseLibs (x : xs)
  | isPrefixOf "-l" x = parseLibs xs . append libraries [drop 2 x]
  | isPrefixOf "-L" x = parseLibs xs . append libraryPath [drop 2 x]
  | otherwise = parseLibs xs . append linkerFlags [x]

parseFlags :: String -> [String]
parseFlags = words' . head . lines

-- | LLVMConfig options.
data Options = Options
  { -- | List of directories where @.pc@ files are searched, corresponding to the @LLVM_CONFIG_PATH@ environment variable
    searchPath :: Maybe [FilePath],
    -- | Return flags appropriate for static linking
    static :: Bool
  }
  deriving (Eq, Show)

-- | Default @llvm-config@ options.
--
-- This function is an alias for `def`.
defaultOptions :: Options
defaultOptions =
  Options
    { searchPath = Nothing,
      static = False
    }

llvmConfig :: Options -> String -> Action (BuildFlags -> BuildFlags)
llvmConfig options pkg = do
  env <- case searchPath options of
    Nothing -> return []
    Just path -> do
      env <- addEnv [("LLVM_CONFIG_PATH", intercalate [searchPathSeparator] path)]
      return [env]
  let flags = if static options then ["--link-static"] else ["--link-shared"]
      llvmconfig which = command ([Traced ""] ++ env) "llvm-config" (flags ++ ["--" ++ which, "--libs", pkg])
  Stdout cxxflags <- llvmconfig "cxxflags"
  Stdout ldflags <- llvmconfig "ldflags"
  Stdout ldsysflags <- llvmconfig "system-libs"
  return
    ( parseCxxflags (parseFlags cxxflags)
        . parseLibs (parseFlags ldflags)
        . parseLibs (parseFlags ldsysflags)
    )

buildDir = "build"

main :: IO ()
main =
  Conc.getNumCapabilities >>= \jobsN -> shakeArgs shakeOptions {shakeFiles = buildDir, shakeThreads = jobsN} do
    let target = OSX.target OSX.macOSX (X86 X86_64)
        toolChain =
          OSX.toolChain
            <$> OSX.getSDKRoot
            <*> (maximum <$> OSX.getPlatformVersions (targetPlatform target))
            <*> pure target
        flags :: Action (BuildFlags -> BuildFlags)
        flags =
          ( return $
              append compilerFlags [(Just Cpp, ["-std=c++11"])]
                >>> append compilerFlags [(Nothing, ["-O3"])]
                >>> append userIncludes ["include"]
                >>> append
                  systemIncludes
                  [ "${llvm-dev}",
                    "${libcxx-dev}",
                    "${zlib-dev}",
                    "${ncurses-dev}"
                  ]
          )
            >>>= (llvmConfig defaultOptions "mcjit")
            >>>= (llvmConfig defaultOptions "x86")
            >>>= (llvmConfig defaultOptions "ipo")
            >>>= (PkgConfig.pkgConfig PkgConfig.defaultOptions "zlib")
            >>>= (PkgConfig.pkgConfig PkgConfig.defaultOptions "ncurses")

    libHobbesA <-
      staticLibrary
        toolChain
        (buildDir </> toBuildPrefix target </> "libHobbes.a")
        flags
        (getDirectoryFiles "" ["lib//*.C"])

    libHobbesD <-
      sharedLibrary
        toolChain
        (buildDir </> toBuildPrefix target </> "libHobbes.dylib")
        flags
        (getDirectoryFiles "" ["lib//*.C"])

    hogD <-
      executable
        toolChain
        (buildDir </> toBuildPrefix target </> "hog-D" <.> exe)
        ( flags
            >>>= ( return $
                     append
                       localLibraries
                       [ libHobbesD
                       ]
                 )
        )
        ( getDirectoryFiles
            ""
            [ "bin/hog//*.C"
            ]
        )

    hogA <-
      executable
        toolChain
        (buildDir </> toBuildPrefix target </> "hog-A" <.> exe)
        ( flags
            >>>= ( return $
                     append
                       localLibraries
                       [ libHobbesA
                       ]
                 )
        )
        ( getDirectoryFiles
            ""
            [ "bin/hog//*.C"
            ]
        )

    hiD <-
      executable
        toolChain
        (buildDir </> toBuildPrefix target </> "hi-D" <.> exe)
        ( flags
            >>>= ( return $
                     append
                       localLibraries
                       [ libHobbesD
                       ]
                       >>> append
                         libraries
                         [ "history",
                           "readline"
                         ]
                       >>> append
                         libraryPath
                         [ "${readline-lib}"
                         ]
                       >>> append
                         systemIncludes
                         [ "${readline-dev}"
                         ]
                 )
        )
        ( getDirectoryFiles
            ""
            [ "bin/hi//*.C"
            ]
        )

    hiA <-
      executable
        toolChain
        (buildDir </> toBuildPrefix target </> "hi-A" <.> exe)
        ( flags
            >>>= ( return $
                     append
                       localLibraries
                       [ libHobbesA
                       ]
                       >>> append
                         libraries
                         [ "history",
                           "readline"
                         ]
                       >>> append
                         libraryPath
                         [ "${readline-lib}"
                         ]
                       >>> append
                         systemIncludes
                         [ "${readline-dev}"
                         ]
                 )
        )
        (getDirectoryFiles "" ["bin/hi//*.C"])

    hobbes_testD <-
      executable
        toolChain
        (buildDir </> toBuildPrefix target </> "hobbes-test-D" <.> exe)
        ( flags
            >>>= ( return $
                     append
                       localLibraries
                       [ libHobbesD
                       ]
                       >>> append
                         userIncludes
                         [ "test"
                         ]
                 )
        )
        ( getDirectoryFiles
            ""
            [ "test//*.C"
            ]
        )

    hobbes_testA <-
      executable
        toolChain
        (buildDir </> toBuildPrefix target </> "hobbes-test-A" <.> exe)
        ( flags
            >>>= ( return $
                     append
                       localLibraries
                       [ libHobbesA
                       ]
                       >>> append
                         userIncludes
                         [ "test"
                         ]
                 )
        )
        ( getDirectoryFiles
            ""
            [ "test//*.C"
            ]
        )

    want [hobbes_testA, hogA, hiA]
