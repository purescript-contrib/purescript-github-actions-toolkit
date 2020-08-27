module GitHub.Actions.Arguments.Optional where

import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Unsafe.Coerce (unsafeCoerce)

-- | All arguments are required and must be specified.
newtype Required r = Required (Variant ( required :: Record r ))

-- | The required row must be specified, and an additional argument symA :: TypA may also be specified
newtype Optional1 required symA typA =
  Optional1 (forall one. Row.Cons symA typA required one => Variant ( required :: Record required, specifyOne :: Record one ))

-- | The required row must be specified, and an additional argument symA :: TypA, symB :: typB may also be specified in order
newtype Optional2 required symA typA symB typB =
  Optional2 (forall one two. Row.Cons symA typA required one => Row.Cons symB typB one two => Variant ( required :: Record required, specifyOne :: Record one, specifyTwo :: Record two ))

-- | The required row must be specified, and an additional argument symA :: TypA, symB :: typB, symC :: typC may also be specified in order
newtype Optional3 required symA typA symB typB symC typC =
  Optional3 (forall one two three. Row.Cons symA typA required one => Row.Cons symB typB one two => Row.Cons symC typC two three => Variant ( required :: Record required, specifyOne :: Record one, specifyTwo :: Record two, specifyThree :: Record three ))

class SpecifyRequired required t | t -> required where
  specifyRequired :: Record required -> t

instance specifyRequiredRequired :: SpecifyRequired r (Required r) where
  specifyRequired given = Required (inj _required given)

else instance specifyRequiredOptional1 :: SpecifyRequired r (Optional1 r symA typA) where
  specifyRequired given = Optional1 (inj _required given)

else instance specifyRequiredFailOptional1
  :: Fail (Above (Above (Text "Improper arguments specified.") ((Beside (Text "Expected: ") (Quote (Record r))))) (Beside (Text "Got: ") (Quote (Record given))))
  => SpecifyRequired given (Optional1 r symA typA) where
  specifyRequired given = unsafeCrashWith "This shouldn't happen."

else instance specifyRequiredOptional2 :: SpecifyRequired r (Optional2 r symA typA symB typB) where
  specifyRequired given = Optional2 (inj _required given)

else instance specifyRequiredFailOptional2
  :: Fail (Above (Above (Text "Improper arguments specified.") ((Beside (Text "Expected: ") (Quote (Record r))))) (Beside (Text "Got: ") (Quote (Record given))))
  => SpecifyRequired given (Optional2 r symA typA symB typB) where
  specifyRequired given = unsafeCrashWith "This shouldn't happen."

else instance specifyRequiredOptional3 :: SpecifyRequired r (Optional3 r symA typA symB typB symC typC) where
  specifyRequired given = Optional3 (inj _required given)

else instance specifyRequiredFailOptional3
  :: Fail (Above (Above (Text "Improper arguments specified.") ((Beside (Text "Expected: ") (Quote (Record r))))) (Beside (Text "Got: ") (Quote (Record given))))
  => SpecifyRequired given (Optional3 r symA typA symB typB symC typC) where
  specifyRequired given = unsafeCrashWith "This shouldn't happen."

class SpecifyOne required symA typA t | t -> required symA typA where
  specifyOne :: forall one. Row.Cons symA typA required one => Record one -> t

instance specifyOneOptional1 :: SpecifyOne required symA typA (Optional1 required symA typA) where
  specifyOne given = Optional1 (inj _specifyOne (unsafeCoerce given))

else instance specifyOneOptional2 :: SpecifyOne required symA typA (Optional2 required symA typA symB typB) where
  specifyOne given = Optional2 (inj _specifyOne (unsafeCoerce given))

else instance specifyOneOption3 :: SpecifyOne required symA typA (Optional3 required symA typA symB typB symC typC) where
  specifyOne given = Optional3 (inj _specifyOne (unsafeCoerce given))

class SpecifyTwo required symA typA symB typB t | t -> required symA typA symB typB where
  specifyTwo :: forall one two. Row.Cons symA typA required one => Row.Cons symB typB one two => Record two -> t

instance specifyTwoOptional2 :: SpecifyTwo required symA typA symB typB (Optional2 required symA typA symB typB) where
  specifyTwo given = Optional2 (inj _specifyTwo (unsafeCoerce given))

else instance specifyTwoOptional3 :: SpecifyTwo required symA typA symB typB (Optional3 required symA typA symB typB symC typC) where
  specifyTwo given = Optional3 (inj _specifyTwo (unsafeCoerce given))

class SpecifyThree required symA typA symB typB symC typC t | t -> required symA typA symB typB symC typC where
  specifyThree :: forall one two three. Row.Cons symA typA required one => Row.Cons symB typB one two => Row.Cons symC typC two three => Record three -> t

instance specifyThreeOptional3 :: SpecifyThree required symA typA symB typB symC typC (Optional3 required symA typA symB typB symC typC) where
  specifyThree given = Optional3 (inj _specifyThree (unsafeCoerce given))

_required = SProxy :: SProxy "required"
_specifyOne = SProxy :: SProxy "specifyOne"
_specifyTwo = SProxy :: SProxy "specifyTwo"
_specifyThree = SProxy :: SProxy "specifyThree"

handleRequired :: forall r o. { required :: Record r -> o } -> Required r -> o
handleRequired elim (Required r) = match elim r

handleOptional1
  :: forall required symA typA all o
   . Row.Cons symA typA required all
  => { required :: Record required -> o, specifyOne :: Record all -> o }
  -> Optional1 required symA typA
  -> o
handleOptional1 elim (Optional1 optional) = match elim optional

handleOptional2
  :: forall required symA typA symB typB one two o
   . Row.Cons symA typA required one
  => Row.Cons symB typB one two
  => { required :: Record required -> o, specifyOne :: Record one -> o, specifyTwo :: Record two -> o}
  -> Optional2 required symA typA symB typB
  -> o
handleOptional2 elim (Optional2 optional) = match elim optional

handleOptional3
  :: forall required symA typA symB typB symC typC one two three o
   . Row.Cons symA typA required one
  => Row.Cons symB typB one two
  => Row.Cons symC typC two three
  => { required :: Record required -> o, specifyOne :: Record one -> o, specifyTwo :: Record two -> o, specifyThree :: Record three -> o }
  -> Optional3 required symA typA symB typB symC typC
  -> o
handleOptional3 elim (Optional3 optional) = match elim optional
