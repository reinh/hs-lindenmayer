{-# LANGUAGE FlexibleContexts #-}

-- | An D0L is a /deterministic/, /context-free/ /L-system/.
--
-- An /L-system/ is a tuple L = (V, ω, P) where
--
--     * V is the alphabet
--     * ω is the axiom (starting state)
--     * P is the set of rules
--
-- Here, V is given by the inhabitants of @a@, ω by an @f@-container
-- of @a@, and P by Kleisli arrows @a -> f a@.
--
-- A /deterministic/ L-system is one where there is only one production
-- rule for each letter in the alphabet.
--
-- A /context-free/ L-system is one where the production rule looks only
-- at the given letter, not at any of its neighbors.
--
-- The original L-system used to model the growth of algae can be given by
--
-- > data Algae = A | B deriving Show
-- >
-- > algae :: D0L [] Algae
-- > algae = D0L rules [B]
-- >   where rules A = [A,B]
-- >         rules B = [A]
--
-- We can demonstrate that the lengths of the generated strings gives
-- the fibonacci sequence:
--
-- >>> print . map (length . axiom) . take 7 . generate $ algae
-- [1,1,2,3,5,8,13]
--
module Lindenmayer.D0L
  ( D0L(..)
  , generate
  , step
  ) where

data D0L m a = D0L
  { rules :: a -> m a
  , axiom :: m a
  }

instance Show (m a) => Show (D0L m a) where
  show (D0L _ ma) = "(D0L undefined " ++ show ma ++ ")"

-- | Generate a list of `D0L` steps starting from an initial system.
generate :: Monad m => D0L m a -> [D0L m a]
generate = iterate step

-- | Apply the rules to each letter of the axiom (in parallel). The
-- resulting string becomes the axiom of a new `D0L` with the same
-- rules.
step :: Monad m => D0L m a -> D0L m a
step (D0L rules state) = D0L rules (state >>= rules)
