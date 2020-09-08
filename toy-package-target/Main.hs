
{-# LANGUAGE PackageImports #-}

module Main where

data Foo = Foo { unFoo :: {-# UNPACK #-} !Int } deriving Show

main :: IO ()
main = print (Foo 10)
