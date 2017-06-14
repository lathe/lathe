-- macros4.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.

{-# LANGUAGE RankNTypes #-}

import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity(Identity))


class N0Functor m where
  n0fmap :: (h0 -> h0') -> (m h0 -> m h0')

-- Laws
law_n0fcompose ::
  (N0Functor m) => (h0 -> h0') -> (h0' -> h0'') -> [m h0 -> m h0'']
law_n0fcompose f g = [n0fmap (g . f), n0fmap g . n0fmap f]
law_n0fid :: (N0Functor m) => [m h0 -> m h0]
law_n0fid = [n0fmap id, id]

class (N0Functor m) => N0Applicative m where
  n0pure :: h0 -> m h0
  n0liftA2 :: (h0u -> h0v -> h0x) -> m h0u -> m h0v -> m h0x

-- This formulation of these laws in terms of `pure`, `fmap`, and
-- `n0liftA2` as opposed to `pure` and (<*>) is thanks in large part
-- to a StackOverflow answer by Cirdec
-- <https://stackoverflow.com/questions/29017633/what-are-the-applicative-functor-laws-in-terms-of-pure-and-lifta2/29018816#29018816>
-- and the `Monoidal` laws from the "Applicative programming with
-- effects" Functional Pearl by Conor McBride and Ross Paterson
-- <http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf>.

-- NOTE: We're encoding a law like "fmap id == id" in a format that
-- helps us check that both sides have the same type:
--
--   law_fmapid :: (Functor f) => [f a -> f a]
--   law_fmapid = [fmap id, id]

-- Naturality laws
law_n0naturalityOfPure ::
  (N0Applicative m) => (h0 -> h0') -> [h0 -> m h0']
law_n0naturalityOfPure f = [n0fmap f . n0pure, n0pure . f]
law_n0naturalityOfLiftA2 ::
  (N0Applicative m) =>
  (h0u -> h0u') ->
  (h0v -> h0v') ->
  (h0u' -> h0v' -> h0x) ->
  (h0x -> h0x') ->
  m h0u ->
  m h0v ->
  [m h0x']
law_n0naturalityOfLiftA2 fu fv f fx mu mv =
  [n0fmap fx (n0liftA2 f (n0fmap fu mu) (n0fmap fv mv)),
    n0liftA2 (\u v -> fx (f (fu u) (fv v))) mu mv]

-- Identity laws
law_n0applicativeLeftIdentity :: (N0Applicative m) => m h0v -> [m h0v]
law_n0applicativeLeftIdentity mv =
  [n0liftA2 (\() v -> v) (n0pure ()) mv, mv]
law_n0applicativeRightIdentity ::
  (N0Applicative m) => m h0u -> [m h0u]
law_n0applicativeRightIdentity mu =
  [n0liftA2 (\u () -> u) mu (n0pure ()), mu]

-- Associativity law
law_n0applicativeAssociativity ::
  (N0Applicative m) =>
  (h0u -> h0v -> h0w -> h0x) -> m h0u -> m h0v -> m h0w -> [m h0x]
law_n0applicativeAssociativity f mu mv mw =
  [n0liftA2 (\(u, v) w -> f u v w) (n0liftA2 (,) mu mv) mw,
    n0liftA2 (\u (v, w) -> f u v w) mu (n0liftA2 (,) mv mw)]

class (N0Applicative m) => N0Monad m where
  n0bind :: (h0 -> m h0') -> (m h0 -> m h0')

-- Naturality law
law_n0monadNaturality ::
  (N0Monad m) =>
  (h0 -> h0') ->
  (h0' -> m h0'') ->
  (m h0'' -> m h0''') ->
  [m h0 -> m h0''']
law_n0monadNaturality f0 g0 h =
  [h . n0bind g0 . n0fmap f0, n0bind (h . g0 . f0)]

-- Identity laws
law_n0monadLeftIdentity :: (N0Monad m) => [m h0 -> m h0]
law_n0monadLeftIdentity = [n0bind n0pure, id]
law_n0monadRightIdentity ::
  (N0Monad m) => (h0 -> m h0') -> [h0 -> m h0']
law_n0monadRightIdentity f0 = [n0bind f0 . n0pure, f0]


class N1Functor m where
  n1fmap ::
    (forall h0 h0'. (h0 -> h0') -> (h1 h0 -> h1' h0')) ->
    (forall h0 h0'. (h0 -> h0') -> (m h1 h0 -> m h1' h0'))

-- Laws
law_n1fcompose ::
  (N1Functor m, N0Functor h1) =>
  (forall h0 h0'. (h0 -> h0') -> (h1 h0 -> h1' h0')) ->
  (h0 -> h0') ->
  (forall h0' h0''. (h0' -> h0'') -> (h1' h0' -> h1'' h0'')) ->
  (h0' -> h0'') ->
  [m h1 h0 -> m h1'' h0'']
law_n1fcompose f1 f0 g1 g0 =
  [n1fmap (\f0' -> g1 id . f1 f0') (g0 . f0),
    n1fmap g1 g0 . n1fmap f1 f0]
law_n1fid :: (N1Functor m, N0Functor h1) => [m h1 h0 -> m h1 h0]
law_n1fid = [n1fmap n0fmap id, id]

class (N1Functor m) => N1Applicative m where
  n1pure :: (forall h0. h0 -> h1 h0) -> (forall h0. h0 -> m h1 h0)
  n1liftA2 ::
    (forall h0u h0v h0x.
      (h0u -> h0v -> h0x) ->
      (h1u h0u -> h1v h0v -> h1x h0x)) ->
    (forall h0u h0v h0x.
      (h0u -> h0v -> h0x) ->
      (m h1u h0u -> m h1v h0v -> m h1x h0x))

-- Naturality laws
law_n1naturalityOfPure ::
  (N1Applicative m) =>
  (forall h0 h0'. (h0 -> h0') -> (h1 h0 -> h1' h0')) ->
  (h0 -> h0') ->
  (forall h0. h0 -> h1 h0) ->
  h0 ->
  [m h1' h0']
law_n1naturalityOfPure f1x f0x f1 f0 =
  [n1fmap f1x f0x (n1pure f1 f0), n1pure (f1x id . f1) (f0x f0)]
law_n1naturalityOfLiftA2 ::
  (N1Applicative m) =>
  (forall h0u h0u'. (h0u -> h0u') -> (h1u h0u -> h1u' h0u')) ->
  (h0u -> h0u') ->
  (forall h0v h0v'. (h0v -> h0v') -> (h1v h0v -> h1v' h0v')) ->
  (h0v -> h0v') ->
  (forall h0u' h0v' h0x.
    (h0u' -> h0v' -> h0x) ->
    (h1u' h0u' -> h1v' h0v' -> h1x h0x)) ->
  (h0u' -> h0v' -> h0x) ->
  (forall h0x h0x'. (h0x -> h0x') -> (h1x h0x -> h1x' h0x')) ->
  (h0x -> h0x') ->
  m h1u h0u ->
  m h1v h0v ->
  [m h1x' h0x']
law_n1naturalityOfLiftA2 f1u f0u f1v f0v f1 f0 f1x f0x mu mv =
  [n1fmap f1x f0x
    (n1liftA2 f1 f0 (n1fmap f1u f0u mu) (n1fmap f1v f0v mv)),
    n1liftA2
      (\f0' u v -> f1x id (f1 f0' (f1u id u) (f1v id v)))
      (\u v -> f0x (f0 (f0u u) (f0v v)))
      mu
      mv]

-- Identity laws
law_n1applicativeLeftIdentity ::
  (N1Applicative m, N0Functor h1v) => m h1v h0v -> [m h1v h0v]
law_n1applicativeLeftIdentity mv =
  [n1liftA2
    (\f0 (Identity u) v -> n0fmap (f0 u) v)
    (\() v -> v)
    (n1pure Identity ())
    mv,
    mv]
law_n1applicativeRightIdentity ::
  (N1Applicative m, N0Functor h1u) => m h1u h0u -> [m h1u h0u]
law_n1applicativeRightIdentity mu =
  [n1liftA2
    (\f0 v (Identity u) -> n0fmap (\v' -> f0 v' u) v)
    (\u () -> u)
    mu
    (n1pure Identity ()),
    mu]

-- Associativity law
law_n1applicativeAssociativity ::
  (N1Applicative m, N0Functor h1v, N0Functor h1u, N0Functor h1w) =>
  (forall h0u h0v h0w h0x.
    (h0u -> h0v -> h0w -> h0x) ->
    (h1u h0u -> h1v h0v -> h1w h0w -> h1x h0x)) ->
  (h0u -> h0v -> h0w -> h0x) ->
  (forall h0u h0v h0uv.
    (h0u -> h0v -> h0uv) ->
    h1u h0u -> h1v h0v -> h1uv h0uv) ->
  (forall h0v h0w h0vw.
    (h0v -> h0w -> h0vw) ->
    h1v h0v -> h1w h0w -> h1vw h0vw) ->
  m h1u h0u ->
  m h1v h0v ->
  m h1w h0w ->
  [m h1x h0x]
data FunctorPair f g a = FunctorPair (f a) (g a)
-- TODO: See if we can formulate this law in a way that typechecks.
law_n1applicativeAssociativity f1 f0 f1uv f1vw mu mv mw =
  [n1liftA2
    undefined
--    (\f0' (Compose (Compose fuv)) w -> fuv f0' w)
--    (\f0' (Compose (Compose fuv)) w -> undefined f0' fuv w)
    (\fuv w -> fuv w)
    (n1liftA2
      undefined
--      (\f0uv u v -> Compose $ f1 (\u v w -> f0uv u v) u v)
--      (\f0uv u v -> Compose $ \w -> f1 (\u v -> 'c' f0uv u v) u v w)
      (\u v w -> f0 u v w)
      mu
      mv)
    mw,
    n1liftA2 undefined (\u fvw -> fvw u) mu (n1liftA2 undefined (\v w u -> f0 u v w) mv mw)]

{-
class (N0Applicative m) => N0Monad m where
  n0bind :: (h0 -> m h0') -> (m h0 -> m h0')

-- Naturality law
law_n0monadNaturality ::
  (N0Monad m) =>
  (h0 -> h0') ->
  (h0' -> m h0'') ->
  (m h0'' -> m h0''') ->
  [m h0 -> m h0''']
law_n0monadNaturality f0 g0 h =
  [h . n0bind g0 . n0fmap f0, n0bind (h . g0 . f0)]

-- Identity laws
law_n0monadLeftIdentity :: (N0Monad m) => [m h0 -> m h0]
law_n0monadLeftIdentity = [n0bind n0pure, id]
law_n0monadRightIdentity ::
  (N0Monad m) => (h0 -> m h0') -> [h0 -> m h0']
law_n0monadRightIdentity f0 = [n0bind f0 . n0pure, f0]
-}


{-
import Data.Bifunctor (Bifunctor, bimap)



newtype ComposeBifunctorFunctor p f a b = ComposeBifunctorFunctor {
  runComposeBifunctorFunctor :: p (f a) (f b)
}
inComposeBifunctorFunctor ::
  (p (f a) (f b) -> q (g c) (g d)) ->
  ComposeBifunctorFunctor p f a b -> ComposeBifunctorFunctor q g c d
inComposeBifunctorFunctor f =
  ComposeBifunctorFunctor . f . runComposeBifunctorFunctor

instance
  (Bifunctor p, Functor f) => Bifunctor (ComposeBifunctorFunctor p f)
  where
  bimap f g = inComposeBifunctorFunctor $ bimap (fmap f) (fmap g)

class N0Functor m where
  n0fmap :: (h0 -> h0') -> (m h0 -> m h0')
  -- laws:
  --   n0fmap (g . f) == n0fmap g . n0fmap f
  --   n0fmap id == id

class (N0Functor m) => N0Applicative m where
  n0pure :: h0 -> m h0
  n0liftA2 :: (h0u -> h0v -> h0x) -> m h0u -> m h0v -> m h0x
  
  -- This formulation of these laws in terms of `pure`, `fmap`, and
  -- `n0liftA2` as opposed to `pure` and (<*>) is thanks in large part
  -- to a StackOverflow answer by Cirdec
  -- <https://stackoverflow.com/questions/29017633/what-are-the-applicative-functor-laws-in-terms-of-pure-and-lifta2/29018816#29018816>
  -- and the `Monoidal` laws from the "Applicative programming with
  -- effects" Functional Pearl by Conor McBride and Ross Paterson
  -- <http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf>.
  
  -- naturality laws:
  --   n0fmap f . n0pure == n0pure . f
  --   n0fmap fx (n0liftA2 f (n0fmap fu mu) (n0fmap fv mv)) ==
  --     n0liftA2 (\u v -> fx (f (fu u) (fv v))) mu mv
  -- identity laws:
  --   n0liftA2 (\() v -> v) (n0pure ()) mv == mv
  --   n0liftA2 (\u () -> u) mu (n0pure ()) == mu
  -- associativity law:
  --   n0liftA2 (\(u, v) w -> f u v w) (n0liftA2 (,) mu mv) mw ==
  --     n0liftA2 (\u (v, w) -> f u v w) mu (n0liftA2 (,) mv mw)

class (N0Applicative m) => N0Monad m where
  n0bind :: (h0 -> m h0') -> (m h0 -> m h0')
  -- naturality law:
  --   h . n0bind g0 . n0fmap f0 == n0bind (h . g0 . f0)
  -- identity laws:
  --   n0bind f0 . n0pure == f0
  --   n0bind n0pure == id


class N1Functor m where
  n1fmap ::
    (h1 (m h1 h0) -> h1' (m h1' h0')) ->
    (h0 -> h0') ->
    (m h1 h0 -> m h1' h0')
  -- laws:
  --   n1fmap (g1 . f1) (g0 . f0) == n1fmap g1 g0 . n0fmap f1 f0
  --   n1fmap id id == id
  --   n1fmap id == n0fmap

class (N1Functor m) => N1Applicative m where
  n1pure :: h1 (m h1 h0) -> m h1 h0
  n1liftA2 ::
    (h1u (m h1u h0u) -> h1v (m h1v h0v) -> h1x (m h1x h0x)) ->
    (h0u -> h0v -> h0w) ->
    (m h1u h0u -> m h1v h0v -> m h1x h0x)
  -- naturality laws:
  --   n1fmap f1 f0 . n1pure == n1pure . f1
  --   n1fmap f1x f0x
  --     (n1liftA2 f1 f0 (n1fmap f1u f0u mu) (n1fmap f1v f0v mv)) ==
  --     n0liftA2
  --       (\u v -> f1x (f1 (f1u u) (f1v v)))
  --       (\u v -> f0x (f0 (f0u u) (f0v v)))
  --       mu
  --       mv
  -- identity laws:
  --   n1liftA2 (\(Const ()) v -> v) (\() v -> v)
  --     (n1pure (Const ()))
  --     mv ==
  --     mv
  --   n1liftA2 (\u (Const ()) -> u) (\u () -> u)
  --     mu
  --     (n1pure (Const ())) ==
  --     mu
  -- associativity law:
  --   n0liftA2
  --     (\(ComposeBifunctorFunctor (u, v)) w -> f1 u v w)
  --     (\(u, v) w -> f0 u v w)
  --     (n0liftA2 (\u v -> ComposeBifunctorFunctor (u, v)) (,) mu mv)
  --     mw ==
  --     n0liftA2
  --       (\u (ComposeBifunctorFunctor (v, w)) -> f1 u v w)
  --       (\u (v, w) -> f0 u v w)
  --       mu
  --       (n0liftA2 (\v w -> ComposeBifunctorFunctor (v, w)) (,)
  --         mv mw)

class (N1Applicative m) => N1Monad m where
  n1bind ::
    (h1 (m h1 h0) -> m h1' h0') ->
    (h0 -> m h1' h0') ->
    (m h1 h0 -> m h1' h0')
  -- naturality law:
  --   h . n1bind g1 g0 . n1fmap f1 f0 ==
  --     n1bind (h . g1 . f1) (h . g0 . f0)
  -- identity laws:
  --   n1bind f1 f0 . n1pure == f1
  --   n1bind f1 f0 . n0pure == f0
  --   n1bind n1pure n0pure == id
  --   n1bind n1pure == n0bind


-- NOTE: Even though we can define certain analogues to `join`, we
-- can't seem to define `h1bind` in terms of `h1join`. This seems to
-- be the best analogue we get.
n1join ::
  (N1Monad m) =>
  (h1 (m h1 (m h1' h0')) -> m h1' h0') ->
  (m h1 (m h1' h0') -> m h1' h0')
n1join f1 = n1bind f1 id

data N2Expr h1 h0
  = N2ExprHole0 h0
  | N2ExprHole1 (h1 (N2Expr h1 h0))
  | N2ExprLayer1 (N2Expr h1 (N2Expr h1 h0))
  | N2ExprLayer2 (N2Expr (N2Expr h1) (N2Expr h1 h0))

instance (N0Functor h1) => N0Functor (N2Expr h1) where
  n0fmap f0 e = case e of
    N2ExprHole0 e' -> N2ExprHole0 $ f0 e'
    N2ExprHole1 e' -> N2ExprHole1 $ n0fmap (n0fmap f0) e'
    N2ExprLayer1 e' -> N2ExprLayer1 $ n0fmap (n0fmap f0) e'
    N2ExprLayer2 e' -> N2ExprLayer2 $ n0fmap (n0fmap f0) e'
instance (N0Functor h1) => N0Applicative (N2Expr h1) where
  n0pure = N2ExprHole0
  n0liftA2 f0 mu mv = case mu of
    N2ExprHole0 u -> n0fmap (f0 u) mv
    N2ExprHole1 mu' -> N2ExprHole1 $ n0fmap recur mu'
    N2ExprLayer1 mu' -> N2ExprLayer1 $ n0fmap recur mu'
    N2ExprLayer2 mu' -> N2ExprLayer2 $ n0fmap recur mu'
    where
    recur mu = n0liftA2 f0 mu mv
instance (N0Functor h1) => N0Monad (N2Expr h1) where
  n0bind f0 m = case m of
    N2ExprHole0 h -> f0 h
    N2ExprHole1 m' -> N2ExprHole1 $ n0fmap (n0bind f0) m'
    N2ExprLayer1 m' -> N2ExprLayer1 $ n0fmap (n0bind f0) m'
    N2ExprLayer2 m' -> N2ExprLayer2 $ n0fmap (n0bind f0) m'
instance N1Functor N2Expr where
  n1fmap f1 f0 e = case e of
    N2ExprHole0 e' -> N2ExprHole0 $ f0 e'
    N2ExprHole1 e' -> N2ExprHole1 $ f1 e'
    -- TODO: Hmm, this isn't working out after all. It seems like the
    -- type signature we'll want for `n1fmap` is as follows:
    --
    --  n1fmap ::
    --    (forall h0 h0'.
    --      (h0 -> h0') ->
    --      (h1 (m h1 h0) -> h1' (m h1' h0'))) ->
    --    (forall h0 h0'.
    --      (h0 -> h0') ->
    --      (m h1 h0 -> m h1' h0'))
    --
    N2ExprLayer1 e' -> N2ExprLayer1 $ n1fmap undefined (n1fmap f1 f0) e' -- TODO
    N2ExprLayer2 e' -> N2ExprLayer2 $ undefined--n0fmap (n1fmap f1 f0) e'

-}