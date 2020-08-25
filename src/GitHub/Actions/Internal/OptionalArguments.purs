module GitHub.Actions.OptionalArguments where

import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match)
import Prim.RowList as RL
import Type.Prelude (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

newtype Required r = Required (Variant ( required :: Record r ))

newtype Optional1 required symA typA = Optional1 (forall all rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA rl) all => Variant ( required :: Record required, specifyOne :: Record all ))

newtype Optional2 required symA typA symB typB
  = Optional2 (forall one two rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA rl) one => ListToRow (RL.Cons symA typA (RL.Cons symB typB rl)) two => Variant ( required :: Record required, specifyOne :: Record one, specifyTwo :: Record two ))

newtype Optional3 required symA typA symB typB symC typC
  = Optional3 (forall one two three rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA rl) one => ListToRow (RL.Cons symA typA (RL.Cons symB typB rl)) two => ListToRow (RL.Cons symA typA (RL.Cons symB typB (RL.Cons symC typC rl))) three => Variant ( required :: Record required, specifyOne :: Record one, specifyTwo :: Record two, specifyThree :: Record three ))

class SpecifyRequired required t where
  specifyRequired :: Record required -> t

instance specifyRequiredRequired :: SpecifyRequired r (Required r) where
  specifyRequired given = Required (inj _required given)

instance specifyRequiredOptional1 :: SpecifyRequired r (Optional1 r symA typA) where
  specifyRequired given = Optional1 (inj _required given)

instance specifyRequiredOptional2 :: SpecifyRequired r (Optional2 r symA typA symB typB) where
  specifyRequired given = Optional2 (inj _required given)

instance specifyRequiredOptional3 :: SpecifyRequired r (Optional3 r symA typA symB typB symC typC) where
  specifyRequired given = Optional3 (inj _required given)

class SpecifyOne required symA typA t | t -> required symA typA where
  specifyOne :: forall rl all. RL.RowToList required rl => ListToRow (RL.Cons symA typA rl) all => Record all -> t

instance specifyOneOptional1 :: SpecifyOne required symA typA (Optional1 required symA typA) where
  specifyOne given = Optional1 (inj _specifyOne (unsafeCoerce given))

instance specifyOneOptional2 :: SpecifyOne required symA typA (Optional2 required symA typA symB typB) where
  specifyOne given = Optional2 (inj _specifyOne (unsafeCoerce given))

instance specifyOneOption3 :: SpecifyOne required symA typA (Optional3 required symA typA symB typB symC typC) where
  specifyOne given = Optional3 (inj _specifyOne (unsafeCoerce given))

class SpecifyTwo required symA typA symB typB t | t -> required symA typA symB typB where
  specifyTwo :: forall two rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA (RL.Cons symB typB rl)) two => Record two -> t

instance specifyTwoOptional2 :: SpecifyTwo required symA typA symB typB (Optional2 required symA typA symB typB) where
  specifyTwo given = Optional2 (inj _specifyTwo (unsafeCoerce given))

instance specifyTwoOptional3 :: SpecifyTwo required symA typA symB typB (Optional3 required symA typA symB typB symC typC) where
  specifyTwo given = Optional3 (inj _specifyTwo (unsafeCoerce given))

class SpecifyThree required symA typA symB typB symC typC t | t -> required symA typA symB typB symC typC where
  specifyThree :: forall three rl. RL.RowToList required rl => ListToRow (RL.Cons symA typA (RL.Cons symB typB (RL.Cons symC typC rl))) three => Record three -> t

instance specifyThreeOptional3 :: SpecifyThree required symA typA symB typB symC typC (Optional3 required symA typA symB typB symC typC) where
  specifyThree given = Optional3 (inj _specifyThree (unsafeCoerce given))

_required = SProxy :: SProxy "required"
_specifyOne = SProxy :: SProxy "specifyOne"
_specifyTwo = SProxy :: SProxy "specifyTwo"
_specifyThree = SProxy :: SProxy "specifyThree"

handleRequired :: forall r o. { required :: Record r -> o } -> Required r -> o
handleRequired elim (Required r) = match elim r

handleOptional1
  :: forall required symA typA all rl o
   . RL.RowToList required rl
  => ListToRow (RL.Cons symA typA rl) all
  => { required :: Record required -> o, specifyOne :: Record all -> o }
  -> Optional1 required symA typA
  -> o
handleOptional1 elim (Optional1 optional) = match elim optional

handleOptional2
  :: forall required symA typA symB typB one two rl o
   . RL.RowToList required rl
  => ListToRow (RL.Cons symA typA rl) one
  => ListToRow (RL.Cons symA typA (RL.Cons symB typB rl)) two
  => { required :: Record required -> o, specifyOne :: Record one -> o, specifyTwo :: Record two -> o}
  -> Optional2 required symA typA symB typB
  -> o
handleOptional2 elim (Optional2 optional) = match elim optional

handleOptional3
  :: forall required symA typA symB typB symC typC one two three rl o
   . RL.RowToList required rl
  => ListToRow (RL.Cons symA typA rl) one
  => ListToRow (RL.Cons symA typA (RL.Cons symB typB rl)) two
  => ListToRow (RL.Cons symA typA (RL.Cons symB typB (RL.Cons symC typC rl))) three
  => { required :: Record required -> o, specifyOne :: Record one -> o, specifyTwo :: Record two -> o, specifyThree :: Record three -> o }
  -> Optional3 required symA typA symB typB symC typC
  -> o
handleOptional3 elim (Optional3 optional) = match elim optional
