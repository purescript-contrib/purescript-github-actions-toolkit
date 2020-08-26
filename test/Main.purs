module Test.Main where

import Prelude

import GitHub.Actions.OptionalArguments (Optional1, Required, handleRequired, specifyOne, specifyRequired)

type Test1Args = Required ( a :: String, b :: Int )

test1 :: Test1Args
test1 = specifyRequired { a: "hello", b: 100000 }

test1Output :: String
test1Output = test1 # handleRequired
  { required: \{ a, b } -> a <> show b
  }

type Test2Args = Optional1 ( a :: String ) "b" String

test2 :: Test2Args
test2 = specifyRequired { a: "hello" }

test2' :: Test2Args
test2' = specifyOne { a: "hello", b: "bye" }
