module Isomorphism where

import Data.Bifunctor
import Data.Void

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) =
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (fmap ab, fmap ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (bimap ab cd, bimap ba dc)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) =
  ( \ac -> \b -> (cd . ac . ba) b,
    \bd -> \a -> (dc . bd . ab) a
  )

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (mab, mba) =
  ( \a -> case (mab (Just a)) of
      Nothing -> case (mab Nothing) of
        Nothing -> error "impossible"
        Just x -> x
      Just x -> x,
    \b -> case (mba (Just b)) of
      Nothing -> error "impossible"
      Just x -> x
  )

-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.

-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
placeholder = replicate 5000 ()
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU =
  ( \eitherListEmpty -> case eitherListEmpty of
      Left x -> Left x
      Right _ -> Left placeholder,
    \eitherListVoid -> case eitherListVoid of
      Left x -> case x of
        p | p == placeholder -> Right ()
        otherwise -> Left x
      Right _ -> Right ()
  )



-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither
-- isLeft (substL isoEU (Right ())) shouldBe True

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm =
  ( \(ab, ba) -> (ba, ab),
    \(ba, ab) -> (ab, ba)
  )
