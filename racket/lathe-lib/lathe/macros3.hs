-- macros3.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}


-- The type (H2Expr h1 h0) represents a
-- higher-quasiquotation-degree-2 strongly typed expression for a
-- family of types `h1` for degree-1 holes and a type `h0` of degree-0
-- holes.
--
data H0Expr
  = H0ExprLayer0 H0Expr
data H1Expr h0
  = H1ExprHole0 h0
  | H1ExprLayer0 (H1Expr h0)
  | H1ExprLayer1 (H1Expr (H1Expr h0))
data H2Expr h1 h0
  = H2ExprHole0 h0
  | H2ExprHole1 (h1 (H2Expr h1 h0))
  | H2ExprLayer0 (H2Expr h1 h0)
  | H2ExprLayer1 (H2Expr h1 (H2Expr h1 h0))
  | H2ExprLayer2 (H2Expr (H2Expr h1) (H2Expr h1 h0))
data H3Expr h2 h1 h0
  = H3ExprHole0 h0
  | H3ExprHole1 (h1 (H3Expr h2 h1 h0))
  | H3ExprHole2 (h2 (H3Expr h2 h1) (H3Expr h2 h1 h0))
  | H3ExprLayer0 (H3Expr h2 h1 h0)
  | H3ExprLayer1 (H3Expr h2 h1 (H3Expr h2 h1 h0))
  | H3ExprLayer2 (H3Expr h2 (H3Expr h2 h1) (H3Expr h2 h1 h0))
  | H3ExprLayer3
      (H3Expr
        (H3Expr h2)
        (H3Expr h2 h1)
        (H3Expr h2 h1 h0))

h1return0 :: h0 -> H1Expr h0
h1return0 = H1ExprHole0
h1map0 :: (h0 -> h0') -> H1Expr h0 -> H1Expr h0'
h1map0 f e = case e of
  H1ExprHole0 h0 -> H1ExprHole0 $ f h0
  H1ExprLayer0 e' -> H1ExprLayer0 $ h1map0 f e'
  H1ExprLayer1 e' -> H1ExprLayer1 $ h1map0 (h1map0 f) e'
h1eval :: H1Expr h0 -> h0
h1eval e = case e of
  H1ExprHole0 h0 -> h0
  H1ExprLayer0 e' -> h1eval e'
  H1ExprLayer1 e' -> h1eval $ h1eval e'
instance (Functor h1) => Functor (H2Expr h1) where
  fmap f e = case e of
    H2ExprHole0 h0 -> H2ExprHole0 $ f h0
    H2ExprHole1 h1 -> H2ExprHole1 $ fmap (fmap f) h1
    H2ExprLayer0 e' -> H2ExprLayer0 $ fmap f e'
    H2ExprLayer1 e' -> H2ExprLayer1 $ fmap (fmap f) e'
    H2ExprLayer2 e' -> H2ExprLayer2 $ fmap (fmap f) e'
instance (Functor h1) => Applicative (H2Expr h1) where
  pure = H2ExprHole0
  f <*> a = case f of
    H2ExprHole0 h0 -> fmap h0 a
    H2ExprHole1 h1 -> H2ExprHole1 $ fmap (<*> a) h1
    H2ExprLayer0 f' -> H2ExprLayer0 $ f' <*> a
    H2ExprLayer1 f' -> H2ExprLayer1 $ fmap (<*> a) f'
    H2ExprLayer2 f' -> H2ExprLayer2 $ fmap (<*> a) f'
instance (Functor h1) => Monad (H2Expr h1) where
  return = pure
  e >>= f = case e of
    H2ExprHole0 h0 -> f h0
    H2ExprHole1 h1 -> H2ExprHole1 $ fmap (>>= f) h1
    H2ExprLayer0 e' -> H2ExprLayer0 $ e' >>= f
    H2ExprLayer1 e' -> H2ExprLayer1 $ fmap (>>= f) e'
    H2ExprLayer2 e' -> H2ExprLayer2 $ fmap (>>= f) e'
h2eval :: (Monad h1) => H2Expr h1 h0 -> h1 h0
h2eval e = case e of
  H2ExprHole0 h0 -> return h0
  H2ExprHole1 h1 -> h2eval =<< h1
  H2ExprLayer0 e' -> h2eval e'
  H2ExprLayer1 e' -> h2eval =<< h2eval e'
  H2ExprLayer2 e' -> h2eval =<< (h2eval $ h2eval e')
class H2Monad h2 where
  h2return0 :: h0 -> h2 h1 h0
  h2bind0 :: (Functor h1) => (h0 -> h2 h1 h0') -> h1 h0 -> h2 h1 h0'
  h2bind1 :: (h1 h0 -> h2 h1' h0') -> h2 h1 h0 -> h2 h1' h0'
instance (H2Monad h2) => H2Monad (H3Expr h2) where
  h2return0 = H3ExprHole0
  h2bind0 f x = H3ExprHole2 $ h2bind1 (h2return0 . H3ExprHole1 . fmap H3ExprHole0) $ h2bind0 (h3eval . f) x
  h2bind1 = undefined  -- TODO
instance (Functor h1) => Functor (H3Expr h2 h1) where
  fmap f e = case e of
    H3ExprHole0 h0 -> H3ExprHole0 $ f h0
    H3ExprHole1 h1 -> H3ExprHole1 $ fmap (fmap f) h1
    -- TODO
    H3ExprHole2 h2 ->
      undefined
--      H3ExprHole2 $
--      h2bind1 (h3eval . H3ExprHole1 . fmap (H3ExprHole0 . fmap f)) h2
    H3ExprLayer0 e' -> undefined--H3ExprLayer0 $ fmap f e'
    H3ExprLayer1 e' -> undefined--H3ExprLayer1 $ fmap f e'
    H3ExprLayer2 e' -> undefined--H3ExprLayer2 $ fmap f e'
    H3ExprLayer3 e' -> undefined--H3ExprLayer3 $ fmap f e'
h3eval :: (H2Monad h2, Functor h1) => H3Expr h2 h1 h0 -> h2 h1 h0
h3eval e = case e of
  H3ExprHole0 h0 -> h2return0 h0
  H3ExprHole1 h1 -> h2bind0 h3eval h1
  H3ExprHole2 h2 -> h2bind1 (h2bind1 (h2bind0 h3eval) . h3eval) h2
  H3ExprLayer0 e' -> h3eval e'
  H3ExprLayer1 e' -> h2bind1 (h2bind0 h3eval) $ h3eval e'
  H3ExprLayer2 e' ->
    h2bind1 (h2bind1 (h2bind0 h3eval) . h3eval) $ h3eval e'
  H3ExprLayer3 e' ->
    h2bind1 (h2bind1 (h2bind0 h3eval) . h3eval) $ h3eval $ h3eval e'
