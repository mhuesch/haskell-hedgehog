-- | Use Hedgehog generators with QuickCheck.
--
module Test.QuickCheck.Hedgehog (
    hedgehog
  , hedgehog'
  ) where

import           Hedgehog
import           Hedgehog.Internal.Gen (evalGen, evalGen')
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (treeValue)

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
