module Test.Main where

import Prelude

import GitHub.Actions.Arguments.Optional (Optional2, Required, handleOptional2, handleRequired, specifyOne, specifyRequired)

type Test1Args = Required ( a :: String, b :: Int )

test1 :: Test1Args
test1 = specifyRequired { a: "hello", b: 100000 }

test1Output :: String
test1Output = test1 # handleRequired
  { required: \{ a, b } -> a <> show b
  }

type Test2Args = Optional2 ( a :: String ) "b" String "c" Int

test2 :: Test2Args
test2 = specifyRequired { a: "hello" }

test2' :: Test2Args
test2' = specifyOne { a: "hello", b: "bye" }

handleTest2 :: String
handleTest2 = test2' # handleOptional2
  { required: \{ a } -> a
  , specifyOne: \{ a, b } -> a <> b
  , specifyTwo: \{ a, b, c } -> a <> b <> show c
  }
