-- macros2.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}


-- We demonstrate a way to extrapolate monads so to operate on kind
-- (* -> *), kind ((* -> *) -> (* -> *)), and so on (but we stop
-- there to avoid exhausting ourselves with clutter). We start with
-- regular monads which operate on kind *, and we call those
-- `H0Monad`.

class H0Monad m0 where
  -- minimal definition:
  --   h0return and (h0bind0 or (h0join0 and h0map0))
  h0map0 :: (a -> a') -> m0 a -> m0 a'
  h0map0 f = h0bind0 (h0return . f)
  h0return :: a -> m0 a
  h0join0 :: m0 (m0 a) -> m0 a
  h0join0 = h0bind0 id
  h0bind0 :: (a -> m0 a') -> m0 a -> m0 a'
  h0bind0 f = h0join0 . h0map0 f
class H1Monad m1 where
  -- minimal definition:
  --   h1return
  --     and (h1bind0 or (h1join0 and h1map0))
  --     and (h1bind1 or (h1join1 and h1map1))
  h1map0 :: (H0Monad m0) => (a -> a') -> m1 m0 a -> m1 m0 a'
  h1map0 f = h1bind0 (h0return . f)
  h1map1 ::
    (H0Monad m0, H0Monad m0') =>
    (forall a. m0 a -> m0' a) -> m1 m0 a -> m1 m0' a
  h1map1 f = h1bind1 (h1return . f)
  h1return :: (H0Monad m0) => m0 a -> m1 m0 a
  h1join0 :: (H0Monad m0) => m1 m0 (m1 m0 a) -> m1 m0 a
  h1join0 = h1bind0 id
  h1join1 :: (H0Monad m0) => m1 (m1 m0) a -> m1 m0 a
  h1join1 = h1bind1 id
  h1bind0 :: (H0Monad m0) => (a -> m1 m0 a') -> m1 m0 a -> m1 m0 a'
  h1bind0 f = h1join0 . h1map0 f
  h1bind1 ::
    (H0Monad m0, H0Monad m0') =>
    (forall a. m0 a -> m1 m0' a) -> m1 m0 a -> m1 m0' a
  h1bind1 f = h1join1 . h1map1 f
instance (H1Monad m1, H0Monad m0) => H0Monad (m1 m0) where
  h0map0 = h1map0
  h0return = h1return . h0return
  h0join0 = h1join0
  h0bind0 = h1bind0
class H2Monad m2 where
  -- minimal definition:
  --   h1return
  --     and (h2bind0 or (h2join0 and h2map0))
  --     and (h2bind1 or (h2join1 and h2map2))
  --     and (h2bind2 or (h2join2 and h2map2))
  h2map0 ::
    (H1Monad m1, H0Monad m0) => (a -> a') -> m2 m1 m0 a -> m2 m1 m0 a'
  h2map0 f = h2bind0 (h0return . f)
  h2map1 ::
    (H1Monad m1, H0Monad m0, H0Monad m0') =>
    (forall a. m0 a -> m0' a) -> m2 m1 m0 a -> m2 m1 m0' a
  h2map1 f = h2bind1 (h1return . f)
  h2map2 ::
    (H1Monad m1, H1Monad m1') =>
    (forall m0 a. (H0Monad m0) => m1 m0 a -> m1' m0 a) ->
    (forall m0 a. (H0Monad m0) => m2 m1 m0 a -> m2 m1' m0 a)
  h2map2 f = h2bind2 (h2return . f)
  h2return :: (H1Monad m1, H0Monad m0) => m1 m0 a -> m2 m1 m0 a
  h2join0 ::
    (H1Monad m1, H0Monad m0) => m2 m1 m0 (m2 m1 m0 a) -> m2 m1 m0 a
  h2join0 = h2bind0 id
  h2join1 ::
    (H1Monad m1, H0Monad m0) => m2 m1 (m2 m1 m0) a -> m2 m1 m0 a
  h2join1 = h2bind1 id
  h2join2 ::
    (H1Monad m1, H0Monad m0) => m2 (m2 m1) m0 a -> m2 m1 m0 a
  h2join2 = h2bind2 id
  h2bind0 ::
    (H1Monad m1, H0Monad m0) =>
    (a -> m2 m1 m0 a') -> m2 m1 m0 a -> m2 m1 m0 a'
  h2bind0 f = h2join0 . h2map0 f
  h2bind1 ::
    (H1Monad m1, H0Monad m0, H0Monad m0') =>
    (forall a. m0 a -> m2 m1 m0' a) -> m2 m1 m0 a -> m2 m1 m0' a
  h2bind1 f = h2join1 . h2map1 f
  h2bind2 ::
    (H1Monad m1, H1Monad m1') =>
    (forall m0 a. (H0Monad m0) => m1 m0 a -> m2 m1' m0 a) ->
    (forall m0 a. (H0Monad m0) => m2 m1 m0 a -> m2 m1' m0 a)
  h2bind2 f = h2join2 . h2map2 f
instance (H2Monad m2, H1Monad m1) => H1Monad (m2 m1) where
  h1map0 = h2map0
  h1map1 = h2map1
  h1return = h2return . h1return
  h1join0 = h2join0
  h1join1 = h2join1
  h1bind0 = h2bind0
  h1bind1 = h2bind1


-- We define some types which can correspond with unresolved identity
-- and composition operations in the "monoid in the category of
-- endofunctors" for higher and higher notions of "endofunctor." For
-- (H1Monad m0) and the type `a`, the monoid identity is (H0Id a), the
-- monoid composition is (H0Meta m0 a), and the notion of
-- "endofunctor" is a natural transformation between (actual)
-- endofunctors of the same category.
--
-- The reason we design these operations to keep the compositions
-- "unresolved" is so that we can pretend there's something in between
-- the two sides of the composition, such as how the closing bracket
-- in the text "[left]right" comes in between the composition of
-- "left" and "right".
--
-- TODO: Prove that these satisfy satisfactory laws to say they're
-- really monoid identity and composition and to be sure we're not
-- making incorrect category theory claims.

data H0Id a = H0Id { h0runId :: a }
instance H0Monad H0Id where
  h0return = H0Id
  h0bind0 f (H0Id a) = f a

data H1Id m0 a = H1Id { h1runId :: m0 a }
instance H1Monad H1Id where
  h1return = H1Id
  h1map0 f (H1Id m) = H1Id $ h0map0 f m
  h1join0 (H1Id m) = H1Id $ h0bind0 h1runId m
  h1bind1 f (H1Id m) = f m

data H2Id (m1 :: (* -> *) -> * -> *) m0 a =
  H2Id { h2runId :: m1 m0 a }
instance H2Monad H2Id where
  h2return = H2Id
  h2map0 f (H2Id m) = H2Id $ h1map0 f m
  h2join0 (H2Id m) = H2Id $ h1bind0 h2runId m
  h2map1 f (H2Id m) = H2Id $ h1map1 f m
  h2join1 (H2Id m) = H2Id $ h1bind1 h2runId m
  h2bind2 f (H2Id m) = f m

data H0Meta m0 a = H0Meta { h0runMeta :: m0 (m0 a) }
instance H1Monad H0Meta where
  h1return = H0Meta . h0return
  h1map0 f (H0Meta m) = H0Meta $ h0map0 (h0map0 f) m
  h1join0 (H0Meta m) = H0Meta $ h0bind0 (h0bind0 h0runMeta) m
  h1bind1 f (H0Meta m) = h0join0 $ f $ h0map0 f m

data H1Meta m1 m0 a = H1Meta { h1runMeta :: m1 (m1 m0) a }
instance H2Monad H1Meta where
  h2return = H1Meta . h1return
  h2map0 f (H1Meta m) = H1Meta $ h1map0 f m
  h2join0 (H1Meta m) = H1Meta $ h1bind0 h1runMeta m
  h2map1 f (H1Meta m) = H1Meta $ h1map1 (h1map1 f) m
  h2join1 (H1Meta m) = H1Meta $ h1bind1 (h1bind1 h1runMeta) m
  h2bind2 f (H1Meta m) = h1join1 $ f $ h1map1 f m


-- The (Balanced m0 a) type represents a document of balanced
-- parentheses, where the document's media (e.g. text) is encoded in
-- an `H0Monad` called `m0` and the document's end-of-file marker is a
-- value of type `a`. We show that `Balanced` itself is an
-- `H1Monad`... or at least we would show this if we properly
-- formulated the analogues to the monad laws for `H1Monad` (TODO).
--
-- TODO: See what it takes to generalize this to a higher degree of
-- quasiquotation. It won't be (Balanced (Balanced m0) a), because
-- that's just two variations of parentheses where the outer variation
-- can't occur anywhere inside the inner variation. We've started on a
-- possible generalization below (starting with `H0BalancedNonMedia`),
-- but since it probably isn't quite right, it's commented out for
-- now.

data Balanced m0 a
  = BalancedNonMedia (BalancedNonMedia m0 a)
  | BalancedMedia (m0 (BalancedNonMedia m0 a))
data BalancedNonMedia m0 a
  = BalancedEnd a
  | BalancedBrackets (Balanced m0 (Balanced m0 a))

instance H1Monad Balanced where
  h1return = BalancedMedia . h0map0 BalancedEnd
  h1bind0 f m = case m of
    BalancedNonMedia m' -> case m' of
      BalancedEnd a -> f a
      BalancedBrackets m'' ->
        BalancedNonMedia $ BalancedBrackets $ h0map0 (h1bind0 f) m''
    BalancedMedia m' -> BalancedMedia $ flip h0bind0 m' $ \m'' ->
      case h1bind0 f (BalancedNonMedia m'') of
        BalancedNonMedia m''' -> h0return m'''
        BalancedMedia m''' -> m'''
  
  -- NOTE: Defining `h1map1`, `h1join1`, and `h1bind1` here is
  -- redundant, but we do it to demonstrate how it's done.
  h1map1 f m = case m of
    BalancedNonMedia m' -> BalancedNonMedia $ h1map1nonMedia f m'
    BalancedMedia m' ->
      BalancedMedia $ h0map0 (h1map1nonMedia f) $ f m'
    where
    h1map1nonMedia ::
      (H0Monad m0, H0Monad m0') =>
      (forall a. m0 a -> m0' a) ->
      BalancedNonMedia m0 a ->
        BalancedNonMedia m0' a
    h1map1nonMedia f m = case m of
      BalancedEnd a -> BalancedEnd a
      BalancedBrackets m' ->
        BalancedBrackets $ h1map1 f $ h1map0 (h1map1 f) m'
  h1join1 m = case m of
    BalancedNonMedia m' -> BalancedNonMedia $ case m' of
      BalancedEnd a -> BalancedEnd a
      BalancedBrackets m'' ->
        BalancedBrackets $ h1join1 $ h1map0 h1join1 m''
    BalancedMedia m' ->
      h1join0 $ h1map0 (h1join1 . BalancedNonMedia) m'
  h1bind1 f m = case m of
    BalancedNonMedia m' -> BalancedNonMedia $ case m' of
      BalancedEnd a -> BalancedEnd a
      BalancedBrackets m'' ->
        BalancedBrackets $ h1bind1 f $ h1map0 (h1bind1 f) m''
    BalancedMedia m' -> h1bind0 (h1bind1 f . BalancedNonMedia) $ f m'

--data H0BalancedNonMedia m0 a
--  = H0BalancedEnd (H0Id a)
--  | H0BalancedBrackets (H0Meta m0 a)
--data H1BalancedNonMedia m1 m0 a
--  = H1BalancedEnd (H1Id m0 a)
--  | H1BalancedBrackets (H1Meta m1 m0 a)
--
--data H0Balanced m0 a
--  = H0BalancedNonMedia (H0BalancedNonMedia (H0Balanced m0) a)
--  | H0BalancedMedia (m0 (H0BalancedNonMedia (H0Balanced m0) a))
--data H1Balanced m1 m0 a
--  = H1BalancedNonMedia
--      (H0BalancedNonMedia (H1Balanced m1 (H0Balanced m0)) a)
--  | H1BalancedMedia
--      (m1 (H1BalancedNonMedia (H1Balanced m1) (H0Balanced m0))
--        (H0BalancedNonMedia (H1Balanced m1 (H0Balanced m0)) a))
