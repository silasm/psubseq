{-# LANGUAGE FlexibleContexts #-}
-- used for psubseq's nightmare type.
module Substring (psubseq) where

import Prelude hiding (length, foldl, foldl1, map, elem)
import Data.Containers
import Data.Monoid
import Data.Semigroup

-- Elements of the subsequence monoid are triples containing the largest left
-- substring, largest right substring, and largest substring occuring in the
-- sequence being searched. Sequence being searched is also contained in the
-- type (potentially this could be two different instances of Sequence but
-- presently the types don't allow this; I want to get it working before I try
-- generalizing it further) so that mappend is a binary operation.
data SubSeqMonoid c v
    = Whole c c
    | Split { search :: c
            , left :: c
            , largest :: c
            , right :: c
            }

-- hmm. This is only a monoid for *fixed* s (searched string); which isn't
-- a problem since s is assumed to be fixed anyways, but this does make
-- defining mempty difficult, because 'Whole mempty mempty' annihilates the result
-- of anything to its right. Semigroups would work fine for this (especially
-- because as soon as we *have* s, it's easy to introduce 'Whole s ""' as
-- mempty, but I don't know how to get 's' soon enough to introduce it to mempty.
--
-- detaching s from the data type would make mappend have a non-compatible
-- type (c -> SubSeqMonoid c -> SubSeqMonoid c -> SubSeqMonoid c), since
-- mappend needs the substring being searched.
--
-- An alternative method would be to just take the larger of two searched
-- strings (right now mappend takes the leftmost since it's assumed to be
-- constant), but this has dumb performance implications.
--
-- I'd like to throw the Reader monad at this problem since it's usually the
-- correct solution where you have unchanging data that's read in several
-- places, but I don't think I could get the types to work out for Monoid OR
-- Semigroup in that case. For now I'll just make it an instance of semigroup.
-- Hopefully I never mess up enough to combine two SubSeqMonoids with a
-- different 's'.
instance (Eq v, Sequence c v) => Semigroup (SubSeqMonoid c v) where
    (<>) = (⊕)

-- (a,b,c) (+) (d,e,f) tries all combinations of (c|d)
-- combine :: SearchMonoid -> SearchMonoid -> SearchMonoid
--
-- TODO: try to turn this into a Reader function somehow. Should be doable,
-- and then I'd be able to turn this whole thing into a solid Monoid and
-- wouldn't have that nasty dangling data everywhere.
infixr 5 ⊕
(⊕) :: (Eq v, Sequence c v) =>
    (SubSeqMonoid c v) ->
    (SubSeqMonoid c v) ->
    (SubSeqMonoid c v)
-- combining two wholes results in another whole iff the two wholes are
-- adjacent in s; otherwise it results in a split for which left, right, and
-- largest subsequences are calculated
--
-- TODO: uses of `psubseq` here cause infinite recursion. Not much gets done.
-- Need to implement base case logic somewhere.
--
-- TODO: this causes a deduction error for God knows what reason. Too tired to
-- work on it now and it's about damn time I pushed this to github, so I'll do
-- that and go to bed.
(Whole s a) ⊕ (Whole _ b)
    | length ((a `mappend` b) `psubseq` s) == length (a `mappend` b) =
                  Whole s
                    (a `mappend` b)
    | otherwise = Split s
                    (leftSubSeq s (a `mappend` b))
                    ((a `mappend` b) `psubseq` s)
                    (rightSubSeq s (a `mappend` b))
-- combining a whole and a split results in a split: the resulting left
-- substring may grow.
(Whole s a) ⊕ b = let left' = leftSubSeq s (a `mappend` left b) in
                    Split s
                    left'
                    (maximize length [left', largest b])
                    (right b)
-- same as the above case but the whole is on the right, so the resulting right
-- substring may grow.
a ⊕ (Whole _ b) = let s      = search a
                      right' = rightSubSeq s (right a `mappend` b)  in
                  Split s
                    (left a)
                    (maximize length [largest a, right'])
                    right'
-- combining two splits preserves the leftmost and rightmost substrings because
-- they can't have grown due to the combination. The middle (left of right and
-- right of left) may have grown from the combination, however, and contend for
-- the position of largest substring in the SubSeqMonoid
a ⊕  b =          let s = search a in
                  Split s
                    (left a)
                    (maximize length $
                      [ largest a
                      , largest b
                      , case Whole s (right a) ⊕  Whole s (left b) of
                          (Whole _ x) -> x
                          (split) -> largest split
                      ]
                    )
                    (right b)

-- returns the first value in the list that maximizes the given function.
-- fails when given empty input because I'm too lazy to deal with Maybe.
maximize :: (Sequence c v, Ord a) => (v -> a) -> c -> v
maximize f = foldl1 (\x y -> if f x > f y then x else y)

-- psubseq generates the largest subsequence by folding over ⊕, a monoidal
-- operation given a fixed s. This should allow some parallelization when
-- I get to it. Just getting it to work sequentially for now.
--
-- type is conceptually the following.
-- psubseq :: (Eq v, Sequence c v) => c -> c -> c
--
-- real type taken from ghci. Just doing (Eq v, Sequence c v) => c -> c -> c
-- should have worked, but didn't, because it couldn't deduce (c ~ c0 v0)
-- at the usage of t, leading to this nightmare of a type.
psubseq
  :: (Eq v, Sequence b v,
      Sequence (c (SubSeqMonoid b v)) (SubSeqMonoid b v),
      CFunctor c (SubSeqMonoid b v), CFunctor c v) =>
     c v -> b -> b
psubseq t s = case foldl (⊕) (Whole s mempty) . map (pure' s) $ t of
    (Whole _ x) -> x
    (split)   -> largest split

-- pure' lifts a single value into a singleton iff it occurs in s, else it lifts
-- it to SubSeqMonoid's mempty under s.
pure' :: (Eq v, Sequence c v) => c -> v -> SubSeqMonoid c v
pure' s x = if x `elem` s then Whole s . singleton $ x else Whole s mempty

-- leftSubSeq gets the largest prefix of a occuring in b.
-- I don't think parallelism will be of much benefit here, so I'll probably make
-- this sequential.
--
-- TODO: implement (duh)
leftSubSeq :: (Eq v, Sequence c v) => c -> c -> c
leftSubSeq = leftSubSeq

-- rightSubSeq gets the largest suffix of a occuring in b.
-- again, sequential is probably best here.
--
-- TODO: implement
rightSubSeq :: (Eq v, Sequence c v) => c -> c -> c
rightSubSeq = rightSubSeq
