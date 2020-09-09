
{-# LANGUAGE PackageImports #-}

module Main where

data Foo = Foo { unFoo :: {-# UNPACK #-} !Int }

main :: IO ()
main = print $ unFoo (Foo 10)
