-- macros4.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.

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
    -- type signature we'll want for `h1fmap` is as follows:
    --
    --  n1fmap ::
    --    (forall h0 h0'.
    --      (h0 -> h0') ->
    --      (h1 (m h1 h0) -> h1' (m h1' h0'))) ->
    --    (forall h0 h0'.
    --      (h0 -> h0') ->
    --      (m h1 h0 -> m h1' h0'))
    --
    -- This is such a complicated type signature that it starts to be
    -- pretty difficult to write down the Functor laws. We're going to
    -- want to typecheck them. For example, let's typecheck something
    -- like (fmap id == id) this way:
    --
    --   frefl0 :: [m a -> m a]
    --   frefl0 = [fmap id, id]
    --
    N2ExprLayer1 e' -> N2ExprLayer1 $ n1fmap undefined (n1fmap f1 f0) e' -- TODO
    N2ExprLayer2 e' -> N2ExprLayer2 $ undefined--n0fmap (n1fmap f1 f0) e'

