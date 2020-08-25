module GitHub.Actions.OptionalArguments where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, case_, inj, on)
import Prim.RowList as RL
import Type.Prelude (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

type Optional1 r symA a
   = forall r1 required
   . IsSymbol symA
  => RL.RowToList r required
  => ListToRow (RL.Cons symA a required) r1
  => Variant ( none :: Record r, one :: Record r1 )

type Optional1'' r symA a all
   = forall required
   . IsSymbol symA
  => RL.RowToList r required
  => ListToRow (RL.Cons symA a required) all
  => Variant ( none :: Record r, one :: Record all )

newtype Required r = Required (Variant ( required :: Record r ))

newtype Optional1''' required symA typA = Optional1''' (forall all rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA rl) all => Variant ( required :: Record required, specifyOne :: Record all ))

newtype Optional2''' required symA typA symB typB
  = Optional2''' (forall one two rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA rl) one => ListToRow (RL.Cons symA typA (RL.Cons symB typB rl)) two => Variant ( required :: Record required, specifyOne :: Record one, specifyTwo :: Record two ))

newtype Optional3''' required symA typA symB typB symC typC
  = Optional3''' (forall one two three rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA rl) one => ListToRow (RL.Cons symA typA (RL.Cons symB typB rl)) two => ListToRow (RL.Cons symA typA (RL.Cons symB typB (RL.Cons symC typC rl))) three => Variant ( required :: Record required, specifyOne :: Record one, specifyTwo :: Record two, specifyThree :: Record three ))

class WithRequired required t where
  required :: Record required -> t

instance requiredRequired :: WithRequired r (Required r) where
  required given = Required (inj _required given)

instance requiredOptional1''' :: WithRequired r (Optional1''' r symA typA) where
  required given = Optional1''' (inj _required given)

instance requiredOptional2''' :: WithRequired r (Optional2''' r symA typA symB typB) where
  required given = Optional2''' (inj _required given)

instance requiredOptional3''' :: WithRequired r (Optional3''' r symA typA symB typB symC typC) where
  required given = Optional3''' (inj _required given)

class WithOptional1 required symA typA t | t -> required symA typA where
  specifyOne :: forall rl all. RL.RowToList required rl => ListToRow (RL.Cons symA typA rl) all => Record all -> t

instance withOptional1Optional1''' :: WithOptional1 required symA typA (Optional1''' required symA typA) where
  specifyOne given = Optional1''' (inj _specifyOne (unsafeCoerce given))

instance withOptional1Optional2''' :: WithOptional1 required symA typA (Optional2''' required symA typA symB typB) where
  specifyOne given = Optional2''' (inj _specifyOne (unsafeCoerce given))

instance withOptional1Option3''' :: WithOptional1 required symA typA (Optional3''' required symA typA symB typB symC typC) where
  specifyOne given = Optional3''' (inj _specifyOne (unsafeCoerce given))

class WithOptional2 required symA typA symB typB t | t -> required symA typA symB typB where
  specifyTwo :: forall two rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA (RL.Cons symB typB rl)) two => Record two -> t

instance withOptional2Optional2''' :: WithOptional2 required symA typA symB typB (Optional2''' required symA typA symB typB) where
  specifyTwo given = Optional2''' (inj _specifyTwo (unsafeCoerce given))

instance withOptional2Optional3''' :: WithOptional2 required symA typA symB typB (Optional3''' required symA typA symB typB symC typC) where
  specifyTwo given = Optional3''' (inj _specifyTwo (unsafeCoerce given))

class WithOptional3 required symA typA symB typB symC typC t | t -> required symA typA symB typB symC typC where
  specifyThree :: forall three rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA (RL.Cons symB typB (RL.Cons symC typC rl))) three => Record three -> t

instance withOptional3Optional3''' :: WithOptional3 required symA typA symB typB symC typC (Optional3''' required symA typA symB typB symC typC) where
  specifyThree given = Optional3''' (inj _specifyThree (unsafeCoerce given))

type Test1Args = Required ( a :: String )

test1 :: Test1Args
test1 = required { a: "hello" }

type Test2Args = Optional1''' ( a :: String ) "b" String

test2 :: Test2Args
test2 = required { a: "hello" }

test2' :: Test2Args
test2' = specifyOne { a: "hello", b: "bye" }

type Test3Args = Optional2''' ( a :: String ) "b" String "c" String

test3 :: Test3Args
test3 = required { a: "hello" }

test3' :: Test3Args
test3' = specifyOne { a: "hello", b: "bye" }

test3''' :: Test3Args
test3''' = specifyTwo { a: "hello", b: "bye", c: "world" }

type Test4Args = Optional3''' ( a :: String ) "b" String "c" String "d" Int

test4 :: Test4Args
test4 = required { a: "hello" }

test4' :: Test4Args
test4' = specifyOne { a: "hello", b: "bye" }

test4''' :: Test4Args
test4''' = specifyTwo { a: "hello", b: "bye", c: "world" }

test4''''' :: Test4Args
test4''''' = specifyThree { a: "hello", b: "bye", c: "world", d: 420 }

class Optional1' (required :: # Type) (symA :: Symbol) typA all | all required -> symA, all required -> typA where
  requiredWithOne :: Record all -> Variant ( none :: Record required, one :: Record all )

instance optional1 :: (IsSymbol symA, RL.RowToList required requiredList, ListToRow (RL.Cons symA typA requiredList) all) => Optional1' required symA typA all where
  requiredWithOne given = inj _one given

{-
class MkOptional1 (r :: # Type) symA a (given :: # Type) where
  mkOptional1 :: Record given -> Optional1 r symA a

instance mkOptional1None :: TypeEquals (RProxy r) (RProxy given) => MkOptional1 r symA a given where
  mkOptional1 given = inj _none given

else instance mkOptional1One :: (RL.RowToList r required, ListToRow (RL.Cons symA a required) r1, TypeEquals (RProxy r1) (RProxy given)) => MkOptional1 r symA a given where
  mkOptional1 given = unsafeCoerce (inj _one given)

else instance mkOptional1Fail :: Fail (Beside (Text "Improper given values") (Quote (Record given))) => MkOptional1 r symA a given where
  mkOptional1 given = unsafeCrashWith "Improper optional values specified - this should not happen."
-}
type Optional2 r symA a symB b
   = forall r1 r2 required
   . IsSymbol symA
  => IsSymbol symB
  => RL.RowToList r required
  => ListToRow (RL.Cons symA a required) r1
  => ListToRow (RL.Cons symA a (RL.Cons symB b required)) r2
  => Variant ( none :: Record r, one :: Record r1, two :: Record r2 )

type Optional3 r symA a symB b symC c
   = forall r1 r2 r3 required
   . IsSymbol symA
  => IsSymbol symB
  => IsSymbol symC
  => RL.RowToList r required
  => ListToRow (RL.Cons symA a required) r1
  => ListToRow (RL.Cons symA a (RL.Cons symB b required)) r2
  => ListToRow (RL.Cons symA a (RL.Cons symB b (RL.Cons symC c required))) r3
  => Variant ( none :: Record r, one :: Record r1, two :: Record r2, three :: Record r3 )

_none = SProxy :: SProxy "none"
_one = SProxy :: SProxy "one"
_two = SProxy :: SProxy "two"
_three = SProxy :: SProxy "three"

_required = SProxy :: SProxy "required"
_specifyOne = SProxy :: SProxy "specifyOne"
_specifyTwo = SProxy :: SProxy "specifyTwo"
_specifyThree = SProxy :: SProxy "specifyThree"

mkOptionalNone
  :: forall v r
   . Record r
  -> Variant ( none :: Record r | v )
mkOptionalNone r = inj _none r

mkOptionalOne
  :: forall r v
   . Record r
  -> Variant ( one :: Record r | v )
mkOptionalOne r = inj _one r

mkOptionalTwo
  :: forall r v
   . Record r
  -> Variant ( two :: Record r | v )
mkOptionalTwo r = inj _two r

mkOptionalThree
  :: forall r v
   . Record r
  -> Variant ( three :: Record r | v )
mkOptionalThree r = inj _three r

handleOptional1
  :: forall symA a o r1 r rl
   . IsSymbol symA
  => RL.RowToList r rl
  => ListToRow (RL.Cons symA a rl) r1
  => { none :: Record r -> o, one :: Record r1 -> o }
  -> Optional1 r symA a
  -> o
handleOptional1 { none, one } option =
  elim option
  where
  elim = case_
    # on _none none
    # on _one one

handleOptional2
  :: forall symA a symB b o r rl r1 r2
   . IsSymbol symA
  => IsSymbol symB
  => RL.RowToList r rl
  => ListToRow (RL.Cons symA a rl) r1
  => ListToRow (RL.Cons symA a (RL.Cons symB b rl)) r2
  => { none :: Record r -> o, one :: Record r1 -> o, two :: Record r2 -> o }
  -> Optional2 r symA a symB b
  -> o
handleOptional2 { none, one, two } option =
  elim option
  where
  elim = case_
    # on _none none
    # on _one one
    # on _two two

handleOptional3
  :: forall symA a symB b symC c o r rl r1 r2 r3
   . IsSymbol symA
  => IsSymbol symB
  => IsSymbol symC
  => RL.RowToList r rl
  => ListToRow (RL.Cons symA a rl) r1
  => ListToRow (RL.Cons symA a (RL.Cons symB b rl)) r2
  => ListToRow (RL.Cons symA a (RL.Cons symB b (RL.Cons symC c rl))) r3
  => { none :: Record r -> o, one :: Record r1 -> o, two :: Record r2 -> o, three :: Record r3 -> o }
  -> Optional3 r symA a symB b symC c
  -> o
handleOptional3 { none, one, two, three } option =
  elim option
  where
  elim = case_
    # on _none none
    # on _one one
    # on _two two
    # on _three three
