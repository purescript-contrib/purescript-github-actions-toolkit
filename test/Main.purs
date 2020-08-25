module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import GitHub.Actions.OptionalArguments (Optional1, Optional2, Optional3, Required, handleOptional1, handleRequired, specifyOne, specifyRequired, specifyThree, specifyTwo)

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

handleTest2 =
  { required: \{ a } -> a
  , specifyOne: \{ a, b } -> a <> b
  }

test2Output :: String
test2Output = handleOptional1 handleTest2 test2

test2Output' :: String
test2Output' = handleOptional1 handleTest2 test2

type Test3Args = Optional2 ( a :: String ) "b" String "c" String

test3 :: Test3Args
test3 = specifyRequired { a: "hello" }

test3' :: Test3Args
test3' = specifyOne { a: "hello", b: "bye" }

test3''' :: Test3Args
test3''' = specifyTwo { a: "hello", b: "bye", c: "world" }

type Test4Args = Optional3 ( a :: String ) "b" String "c" String "d" Int

test4 :: Test4Args
test4 = specifyRequired { a: "hello" }

test4' :: Test4Args
test4' = specifyOne { a: "hello", b: "bye" }

test4''' :: Test4Args
test4''' = specifyTwo { a: "hello", b: "bye", c: "world" }

test4'''' :: Test4Args
test4'''' = specifyThree { a: "hello", b: "bye", c: "world", d: 420 }


main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
