module GitHub.Actions.OptionalArguments where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, case_, inj, on)

type Optional2 a b = Variant ( none :: Unit, one :: a, two :: a /\ b )

type Optional3 a b c = Variant ( none :: Unit, one :: a, two :: a /\ b, three :: a /\ b /\ c )

_none = SProxy :: SProxy "none"
_one = SProxy :: SProxy "one"
_two = SProxy :: SProxy "two"
_three = SProxy :: SProxy "three"

mkOptionalNone :: forall v. Variant ( none :: Unit | v )
mkOptionalNone = inj _none unit

mkOptionalOne :: forall v a. a -> Variant ( one :: a | v )
mkOptionalOne a = inj _one a

mkOptionalTwo :: forall v a b. a -> b -> Variant ( two :: a /\ b | v )
mkOptionalTwo a b = inj _two (a /\ b)

mkOptionalThree :: forall v a b c. a -> b -> c -> Variant ( three :: a /\ b /\ c | v )
mkOptionalThree a b c = inj _three (a /\ b /\ c)

handleOptional2 :: forall a b o. { none :: Unit -> o, one :: a -> o, two :: a /\ b -> o } -> Optional2 a b -> o
handleOptional2 { none, one, two } =
  case_
    # on _none none
    # on _one one
    # on _two two

handleOptional3 :: forall a b c o. { none :: Unit -> o, one :: a -> o, two :: a /\ b -> o, three :: a /\ b /\ c -> o } -> Optional3 a b c -> o
handleOptional3 { none, one, two, three } =
  case_
    # on _none none
    # on _one one
    # on _two two
    # on _three three
