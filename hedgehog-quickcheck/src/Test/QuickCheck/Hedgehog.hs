-- | Use Hedgehog generators with QuickCheck.
--
module Test.QuickCheck.Hedgehog (
    hedgehog
  , hedgehog'
  , ensureEvalGenNotBorked
  ) where

import qualified Data.Maybe as Maybe

import           Hedgehog
import           Hedgehog.Internal.Gen (evalGen, evalGen', evalGenT)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (Tree, treeValue)
import qualified Hedgehog.Internal.Tree as Tree

import qualified Test.QuickCheck as QuickCheck


genSeed :: QuickCheck.Gen Seed
genSeed =
  Seed.from <$> QuickCheck.arbitraryBoundedIntegral

-- | Create a QuickCheck 'QuickCheck.Gen' from a Hedgehog 'Gen'.
--
--   /Note that this conversion does not preserve shrinking. There is currently/
--   /no way to use Hedgehog's shrinking capability inside QuickCheck./
--
hedgehog :: Gen a -> QuickCheck.Gen a
hedgehog gen =
  let
    loop n =
      if n <= 0 then
        QuickCheck.discard
      else do
        seed <- genSeed
        size <- QuickCheck.sized (pure . fromIntegral)
        case evalGen size seed gen of
          Nothing ->
            loop (n - 1)
          Just x ->
            pure $ treeValue x
  in
    loop (100 :: Int)


hedgehog' :: String -> Gen a -> QuickCheck.Gen a
hedgehog' str gen =
  let
    loop n =
      if n <= 0 then
        QuickCheck.discard
      else do
        seed <- genSeed
        size <- QuickCheck.sized (pure . fromIntegral)
        case evalGen' str size seed gen of
          Nothing ->
            loop (n - 1)
          Just x ->
            pure $ treeValue x
  in
    loop (100 :: Int)


ensureEvalGenNotBorked :: (Eq a, Show a) => String -> Gen a -> QuickCheck.Property
ensureEvalGenNotBorked str gen = loop 100

 where
  loop :: Int -> QuickCheck.Property
  loop n =
    if n <= 0 then
      QuickCheck.discard
    else QuickCheck.forAll genSeedSize $ \(seed, size) ->
      (QuickCheck..&.)
        (case evalMaybeGen size seed gen of
           Nothing -> QuickCheck.property True
           Just x -> QuickCheck.counterexample str $
                       (treeValue x) QuickCheck.=/= Nothing)
        (loop (n - 1))

  evalMaybeGen :: Size -> Seed -> Gen a -> Maybe (Tree (Maybe a))
  evalMaybeGen size seed =
    Tree.filter Maybe.isJust .
    evalGenT size seed

  genSize = QuickCheck.sized (pure . fromIntegral)

  genSeedSize = (,) <$> genSeed <*> genSize
