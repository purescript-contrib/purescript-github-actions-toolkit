module GitHub.Actions.OptionalArguments where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, case_, inj, on)
import Prim.RowList as RL
import Type.Prelude (class ListToRow)

type Optional1 r symA a
   = forall r1 required
   . IsSymbol symA
  => RL.RowToList r required
  => ListToRow (RL.Cons symA a required) r1
  => Variant ( none :: Record r, one :: Record r1 )

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

mkOptionalNone :: forall v. Variant ( none :: Unit | v )
mkOptionalNone = inj _none unit

mkOptionalOne
  :: forall v symA a r rl
   . ListToRow (RL.Cons symA a r) rl
  => Record rl
  -> Variant ( one :: Record rl | v )
mkOptionalOne = inj _one

mkOptionalTwo
  :: forall v symA a symB b rl
   . ListToRow (RL.Cons symA a (RL.Cons symB b RL.Nil)) rl
  => Record rl
  -> Variant ( two :: Record rl | v )
mkOptionalTwo = inj _two

mkOptionalThree
  :: forall v symA a symB b symC c rl
   . ListToRow (RL.Cons symA a (RL.Cons symB b (RL.Cons symC c RL.Nil))) rl
  => Record rl
  -> Variant ( three :: Record rl | v )
mkOptionalThree = inj _three

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
