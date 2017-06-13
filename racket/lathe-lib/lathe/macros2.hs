-- macros2.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

import Data.Functor.Compose (Compose(Compose))
import Data.Map (Map)



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
instance {-# OVERLAPPABLE #-}
  (H1Monad m1, H0Monad m0) => H0Monad (m1 m0)
  where
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
instance {-# OVERLAPPABLE #-}
  (H2Monad m2, H1Monad m1) => H1Monad (m2 m1)
  where
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
-- possible generalization below (starting with `H0Balanced`), but we
-- haven't quite managed to implement an instance for
-- (H2Monad H1Balanced).

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
  -- redundant, but we do it anyway as an exercise.
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


data H0Balanced m0 a
  = H0BalancedNonMedia (H0BalancedNonMedia m0 a)
  | H0BalancedMedia (m0 (H0BalancedNonMedia m0 a))
data H0BalancedNonMedia m0 a
  = H0BalancedEnd a
  | H0BalancedBrackets (H0Balanced m0 (H0Balanced m0 a))

data H1Balanced m1 m0 a
  = H1BalancedNonMedia (H1BalancedNonMedia m1 m0 a)
  | H1BalancedMedia (m1 m0 (H1BalancedNonMedia m1 m0 a))
data H1BalancedNonMedia m1 m0 a
  = H1BalancedEnd (m0 a)
  | H1BalancedBrackets (H1Balanced m1 (H1Balanced m1 m0) a)

data H2Balanced m2 m1 m0 a
  = H2BalancedNonMedia (H2BalancedNonMedia m2 m1 m0 a)
  | H2BalancedMedia (m2 m1 m0 (H2BalancedNonMedia m2 m1 m0 a))
data H2BalancedNonMedia m2 m1 m0 a
  = H2BalancedEnd (m1 m0 a)
  | H2BalancedBrackets (H2Balanced m2 (H2Balanced m2 m1) m0 a)

instance H2Monad H1Balanced where
  
  h2return = H1BalancedMedia . h1map0 (H1BalancedEnd . h0return)
  
  h2bind0 f m = case m of
    H1BalancedNonMedia m' -> case m' of
      H1BalancedEnd m'' -> h1fromMedia $ h1return $ h0map0 f m''
      H1BalancedBrackets m'' ->
        H1BalancedNonMedia $ H1BalancedBrackets $
        h2bind0 (h2map1 h1return . f) m''
    H1BalancedMedia m' ->
      h1fromMedia $ h1map0 (h1bind0 f . H1BalancedNonMedia) m'
  
  h2bind1 f m = case m of
    H1BalancedNonMedia m' -> case m' of
      H1BalancedEnd m'' -> f m''
      H1BalancedBrackets m'' ->
        H1BalancedNonMedia $ H1BalancedBrackets $
        h2map1 (h2bind1 f) m''
    H1BalancedMedia m' ->
      -- TODO: Finish implementing this if possible. If it's not
      -- possible, then we need to reconsider our approach here.
      undefined
  
  h2bind2 f m = case m of
    H1BalancedNonMedia m' -> H1BalancedNonMedia $ case m' of
      H1BalancedEnd m'' -> H1BalancedEnd m''
      H1BalancedBrackets m'' ->
        H1BalancedBrackets $ h2bind2 f $ h2map1 (h2bind2 f) m''
    H1BalancedMedia m' ->
      h2bind0 (h2bind2 f . H1BalancedNonMedia) $ f m'

h1fromMedia ::
  (H1Monad m1, H0Monad m0) =>
  m1 m0 (H1Balanced m1 m0 a) -> H1Balanced m1 m0 a
h1fromMedia = H1BalancedMedia . h1bind0 returnNonMedia
  where
  returnNonMedia ::
    (H1Monad m1, H0Monad m0) =>
    H1Balanced m1 m0 a -> m1 m0 (H1BalancedNonMedia m1 m0 a)
  returnNonMedia m = case m of
    H1BalancedNonMedia m' -> h0return m'
    H1BalancedMedia m' -> m'



-- The type (H2MExpr s m h1 h0) represents a
-- higher-quasiquotation-degree-2 `Map`-based expression for a
-- serializable key type `s` (typically something like `String`), a
-- syntax monad `m`, a type `h1` of stand-ins for degree-1 holes, and
-- a type `h0` of degree-0 holes. These `Map`-based expressions aren't
-- very strongly typed because there's no compile-time guarantee that
-- every key appearing in a hole position will have an entry in the
-- corresponding `Map`.
--
-- We arrived at this by considering an example:
--
-- Suppose we have a pseudocode language where Lisp s-expressions are
-- the syntax monad, the characters ` , represent quasiquotation of
-- degree 0, and the characters ^ $ represent quasiquotation of
-- degree 1.
--
-- A degree-0-quasiquotation-shaped data structure (which we'll also
-- call a degree-1 expression) looks like this, where the symbols `a`
-- and `b` help to indicate how the structure nests and `--`
-- represents a hole:
--
--   `(a `(b ,(a ,(--) a) (b) b) a ,(--) a (a) a)
--
-- The `b` structure nested inside looks like this:
--
--   `(b ,(--) (b) b)
--
-- A degree-1-quasiquotation-shaped data structure (aka a degree-2
-- expression) looks like this:
--
--   ^`(a
--       ^`(b
--           $`(a
--               $`(--)
--               a ,(b (b) b) a ,(b) a `(a) a (a) a)
--           b ,(a) b `(b) b (b) b)
--       a ,(--) a `(a) a (a) a)
--
-- The `b` structure nested inside that one looks like this:
--
--   ^`(b
--       $`(-- ,(b (b) b) ,(b))
--       b ,(--) b `(b) b (b) b)
--
-- This time we have two kinds of holes: An occurrence of , introduces
-- a hole of degree 0 which can be filled with an s-expression (aka a
-- degree-0 expression). An occurrence of $` introduces a hole of
-- degree 1, and occurrences of , inside that hole leave it again. A
-- hole of degree 1 can be filled with a quasiquotation of degree 0
-- (aka an expression of degree 1).
--
-- When we have a hole of degree 1 that hasn't been filled yet, the
-- unquotes inside it are rather orphaned. To tell them apart, we may
-- want to give them `String` labels. When we do, the kind of data
-- that can be inserted into that hole becomes more specific: Instead
-- of just any degree-0 quasiquotation, it should be a degree-0
-- quasiquotation where the degree-0 holes are represented by `String`
-- labels corresponding to the labels of our orphaned unquotes.
--
-- So this means the data type acts as a sort of container of holes,
-- and oftentimes we'll want it to contain strings in those holes.
--
-- We also know a lot about the structure of this data now: For every
-- degree, an expression of that degree is shaped like an s-expression
-- which can contain holes of strictly lower degrees. In the same way,
-- a hole of some degree has orphaned sections containing expressions
-- of every strictly lower degree.
--
-- There's something else that can appear wherever a hole can appear:
-- A *nested expression* of the same or lesser degree. When this
-- nested expression reaches a hole, it resumes the original
-- expression. So when a nested expression appears in our data
-- structure, there's some additional data that appears in the holes
-- within that.
--
-- (In this example, we didn't consider ( ) to be a variant of
-- quasiquotation because we wouldn't have multiple orphaned closing
-- parens to differentiate with labels. However, there should be
-- nothing stopping us from using a label anyway, and hence nothing
-- stopping us from designing a language where the syntax monad is
-- (Writer String) and the characters ( ) represent degree-0
-- quasiquotation.)
--
data H0MExpr s m
  = H0MExprMedia (m (H0MExprNonMedia s m))
data H0MExprNonMedia s m
  = H0MExprLayer0 (H0MExpr s m)
data H1MExpr s m h0
  = H1MExprMedia (m (H1MExprNonMedia s m h0))
data H1MExprNonMedia s m h0
  = H1MExprHole0 h0
  | H1MExprLayer0 (H1MExpr s m h0)
  | H1MExprLayer1 (H1MExpr s m s)
      (Map s (H1MExpr s m h0))
data H2MExpr s m h1 h0
  = H2MExprMedia (m (H2MExprNonMedia s m h1 h0))
data H2MExprNonMedia s m h1 h0
  = H2MExprHole0 h0
  | H2MExprHole1 h1
      (Map s (H2MExpr s m h1 h0))
  | H2MExprLayer0 (H2MExpr s m h1 h0)
  | H2MExprLayer1 (H2MExpr s m h1 s)
      (Map s (H2MExpr s m h1 h0))
  | H2MExprLayer2 (H2MExpr s m s s)
      (Map s (H2MExpr s m h1 h0))
      (Map s (H2MExpr s m h1 s))
data H3MExpr s m h2 h1 h0
  = H3MExprMedia (m (H3MExprNonMedia s m h2 h1 h0))
data H3MExprNonMedia s m h2 h1 h0
  = H3MExprHole0 h0
  | H3MExprHole1 h1
      (Map s (H3MExpr s m h2 h1 h0))
  | H3MExprHole2 h2
      (Map s (H3MExpr s m h2 h1 h0))
      (Map s (H3MExpr s m h2 h1 s))
  | H3MExprLayer0 (H3MExpr s m h2 h1 h0)
  | H3MExprLayer1 (H3MExpr s m h2 h1 s)
      (Map s (H3MExpr s m h2 h1 h0))
  | H3MExprLayer2 (H3MExpr s m h2 s s)
      (Map s (H3MExpr s m h2 h1 h0))
      (Map s (H3MExpr s m h2 h1 s))
  | H3MExprLayer3 (H3MExpr s m s s s)
      (Map s (H3MExpr s m h2 h1 h0))
      (Map s (H3MExpr s m h2 h1 s))
      (Map s (H3MExpr s m h2 s s))

-- The type (HDExpr s m) represents a higher quasiquotation expression
-- of dynamic degree for a serializable key type `s` (typically
-- something like `String`) and a syntax monad `m`. This is even less
-- strongly-typed than the `H0MExpr` family, because this doesn't even
-- statically guarantee that holes will be filled in with expressions
-- of appropriate degree, nor that the holes will have strictly lesser
-- degree than the expression they appear in. These properties must be
-- enforced dynamically to keep the higher quasiquotation structure
-- well-formed. However, this representation will be useful for
-- expressing algorithms that operate on expressions of arbitrary
-- higher quasiquotation degree.
--
-- Note that if any holes appear in a higher quasiquotation expression
-- encoded this way, they must be represented using the same key type
-- that the internal (filled) holes use. So, metadata associated with
-- those holes may need to be tracked in an external `Map`.
--
-- We arrived at this design by conflating the constructors of the
-- `H0MExpr` family so that they could be differentiated using nothing
-- but list length.
--
data HDExpr s m
  = HDExprMedia (m (HDExprNonMedia s m))
data HDExprNonMedia s m
  = HDExprHole s [Map s (HDExpr s m)]
  | HDExprLayer (HDExpr s m) [Map s (HDExpr s m)]

-- The type (H2TExpr m h1 h0) represents a
-- higher-quasiquotation-degree-2 strongly typed expression for a
-- syntax monad `m`, a family of types `h1` for degree-1 holes, and a
-- type `h0` of degree-0 holes.
--
-- We arrived at these by simplifying the `H0MExpr` family to remove
-- all uses of `s`.
--
data H0TExpr m
  = H0TExprMedia (m (H0TExprNonMedia m))
data H0TExprNonMedia m
  = H0TExprLayer0 (H0TExpr m)
data H1TExpr m h0
  = H1TExprMedia (m (H1TExprNonMedia m h0))
data H1TExprNonMedia m h0
  = H1TExprHole0 h0
  | H1TExprLayer0 (H1TExpr m h0)
  | H1TExprLayer1 (H1TExpr m (H1TExpr m h0))
data H2TExpr m h1 h0
  = H2TExprMedia (m (H2TExprNonMedia m h1 h0))
data H2TExprNonMedia m h1 h0
  = H2TExprHole0 h0
  | H2TExprHole1 (h1 (H2TExpr m h1 h0))
  | H2TExprLayer0 (H2TExpr m h1 h0)
  | H2TExprLayer1 (H2TExpr m h1 (H2TExpr m h1 h0))
  | H2TExprLayer2 (H2TExpr m (H2TExpr m h1) (H2TExpr m h1 h0))
data H3TExpr m h2 h1 h0
  = H3TExprMedia (m (H3TExprNonMedia m h2 h1 h0))
data H3TExprNonMedia m h2 h1 h0
  = H3TExprHole0 h0
  | H3TExprHole1 (h1 (H3TExpr m h2 h1 h0))
  | H3TExprHole2 (h2 (H3TExpr m h2 h1) (H3TExpr m h2 h1 h0))
  | H3TExprLayer0 (H3TExpr m h2 h1 h0)
  | H3TExprLayer1 (H3TExpr m h2 h1 (H3TExpr m h2 h1 h0))
  | H3TExprLayer2
      (H3TExpr m h2
        (H3TExpr m h2 h1)
        (H3TExpr m h2 h1 h0))
  | H3TExprLayer3
      (H3TExpr m
        (H3TExpr m h2)
        (H3TExpr m h2 h1)
        (H3TExpr m h2 h1 h0))

-- TODO: There's an encouraging resemblance between the definitions of
-- `H1TExpr` and `Balanced`. So now we're seeing if we can make
-- instances like so:
--
--   instance (Monad m) => H0Monad (H1TExpr m)
--   instance (Monad m) => H1Monad (H2TExpr m)
--   instance (Monad m) => H2Monad (H3TExpr m)

newtype H1TExprList a = H1TExprList { deH1TExprList :: H1TExpr [] a }
asH1TExprList ::
  (H1TExprList a -> H1TExprList b) -> H1TExpr [] a -> H1TExpr [] b
asH1TExprList f = deH1TExprList . f . H1TExprList
-- NOTE: Welp, overlapping instances don't help because we get
-- instance incoherence anyway. So we define a newtype specialized to
-- lists as a proof of concept.
--instance {-# OVERLAPPING #-} (Monad m) => H0Monad (H1TExpr m) where
instance H0Monad H1TExprList where
  h0return = H1TExprList . H1TExprMedia . return . H1TExprHole0
  h0map0 f (H1TExprList (H1TExprMedia m)) =
    H1TExprList $ H1TExprMedia $ flip fmap m $ \m' -> case m' of
      H1TExprHole0 h0 -> H1TExprHole0 $ f h0
      H1TExprLayer0 layer ->
        H1TExprLayer0 $ asH1TExprList (h0map0 f) layer
      H1TExprLayer1 layer ->
        H1TExprLayer1 $
        (asH1TExprList $ h0map0 $ asH1TExprList $ h0map0 f) $
        layer
  h0join0 (H1TExprList (H1TExprMedia m)) =
    H1TExprList $ H1TExprMedia $ m >>= \m' -> deMedia $ case m' of
      H1TExprHole0 h0 -> h0
      H1TExprLayer0 layer -> h0join0 $ H1TExprList layer
      H1TExprLayer1 layer ->
        h0bind0 (h0join0 . H1TExprList) $ H1TExprList layer
    where
    deMedia :: H1TExprList a -> [H1TExprNonMedia [] a]
    deMedia (H1TExprList (H1TExprMedia nonMedia)) = nonMedia

newtype H2TExprList m0 a =
  H2TExprList { deH2TExprList :: H2TExpr [] m0 a }
asH2TExprList ::
  (H2TExprList m0a a -> H2TExprList m0b b) ->
  (H2TExpr [] m0a a -> H2TExpr [] m0b b)
asH2TExprList f = deH2TExprList . f . H2TExprList
-- NOTE: Welp, overlapping instances don't help because we get
-- instance incoherence anyway. So we define a newtype specialized to
-- lists as a proof of concept.
--instance {-# OVERLAPPING #-} (Monad m) => H1Monad (H2TExpr m) where
instance H1Monad H2TExprList where
  h1return =
    H2TExprList . H2TExprMedia . return . H2TExprHole1 .
    h0bind0 (h0return . H2TExprMedia . return . H2TExprHole0)
  h1map0 f (H2TExprList (H2TExprMedia m)) =
    H2TExprList $ H2TExprMedia $ flip fmap m $ \m' -> case m' of
      H2TExprHole0 h0 -> H2TExprHole0 $ f h0
      H2TExprHole1 h1 ->
        H2TExprHole1 $ h0map0 (asH2TExprList (h1map0 f)) h1
      H2TExprLayer0 layer ->
        H2TExprLayer0 $ asH2TExprList (h1map0 f) layer
      H2TExprLayer1 layer -> undefined -- TODO
      H2TExprLayer2 layer ->
        H2TExprLayer2 $
        -- TODO: Hmm, this doesn't even seem possible. Perhaps the
        -- design of the `H0Monad` family isn't actually helpful for
        -- this data structure.
        undefined $
        layer
  h1join0 (H2TExprList (H2TExprMedia m)) = undefined  -- TODO
  h1join1 (H2TExprList (H2TExprMedia m)) = undefined  -- TODO

h2texprReturn0 :: (Monad m) => h0 -> H2TExpr m h1 h0
h2texprReturn0 = H2TExprMedia . return . H2TExprHole0
h2texprReturn1 :: (Monad m, Functor h1) => h1 h0 -> H2TExpr m h1 h0
h2texprReturn1 =
  H2TExprMedia . return . H2TExprHole1 . fmap h2texprReturn0
h2texprMap0 ::
  (Monad m) =>
  (forall h0 h0'. (h0 -> h0') -> h1 h0 -> h1 h0') ->
  ((h0 -> h0') -> H2TExpr m h1 h0 -> H2TExpr m h1 h0')
h2texprMap0 fmaph1 f (H2TExprMedia m) =
  H2TExprMedia $ flip fmap m $ \m' -> case m' of
    H2TExprHole0 h0 -> H2TExprHole0 $ f h0
    H2TExprHole1 h1 -> H2TExprHole1 $ fmaph1 (h2texprMap0 fmaph1 f) h1
    H2TExprLayer0 layer -> H2TExprLayer0 $ h2texprMap0 fmaph1 f layer
    H2TExprLayer1 layer ->
      H2TExprLayer1 $ h2texprMap0 fmaph1 (h2texprMap0 fmaph1 f) layer
    H2TExprLayer2 layer ->
      H2TExprLayer2 $
      h2texprMap0 (h2texprMap0 fmaph1) (h2texprMap0 fmaph1 f) layer
h2texprBind0 ::
  (Monad m) =>
  (forall h0 h0'. (h0 -> h0') -> h1 h0 -> h1 h0') ->
  ((h0 -> H2TExpr m h1 h0') -> H2TExpr m h1 h0 -> H2TExpr m h1 h0')
h2texprBind0 fmaph1 f (H2TExprMedia m) =
  H2TExprMedia $ m >>= \m' -> case m' of
    H2TExprHole0 h0 -> let H2TExprMedia h0' = f h0 in h0'
    H2TExprHole1 h1 ->
      return $ H2TExprHole1 $ fmaph1 (h2texprBind0 fmaph1 f) h1
    H2TExprLayer0 layer ->
      return $ H2TExprLayer0 $ h2texprBind0 fmaph1 f layer
    H2TExprLayer1 layer ->
      return $ H2TExprLayer1 $
      h2texprBind0 fmaph1 (h2texprReturn0 . h2texprBind0 fmaph1 f)
        layer
    H2TExprLayer2 layer ->
      return $ H2TExprLayer2 $
      h2texprBind0 (h2texprBind0 fmaph1 . (h2texprReturn0 .))
        (h2texprReturn0 . h2texprBind0 fmaph1 f)
        layer
h2texprJoin0 ::
  (Monad m) =>
  (forall h0 h0'. (h0 -> h0') -> h1 h0 -> h1 h0') ->
  (H2TExpr m h1 (H2TExpr m h1 h0) -> H2TExpr m h1 h0)
h2texprJoin0 fmaph1 (H2TExprMedia m) =
  H2TExprMedia $ m >>= \m' -> case m' of
    H2TExprHole0 h0 -> let H2TExprMedia h0' = h0 in h0'
    H2TExprHole1 h1 ->
      return $ H2TExprHole1 $ fmaph1 (h2texprJoin0 fmaph1) h1
    H2TExprLayer0 layer ->
      return $ H2TExprLayer0 $ h2texprJoin0 fmaph1 layer
    H2TExprLayer1 layer ->
      return $ H2TExprLayer1 $
      h2texprMap0 fmaph1 (h2texprJoin0 fmaph1) layer
    H2TExprLayer2 layer ->
      return $ H2TExprLayer2 $
      h2texprMap0 (h2texprMap0 fmaph1) (h2texprJoin0 fmaph1) layer
h2texprBind1 ::
  (Monad m) =>
  (forall h0 h0'. (h0 -> h0') -> h1 h0 -> h1 h0') ->
  ((h1 h0 -> H2TExpr m h1' h0) -> H2TExpr m h1 h0 -> H2TExpr m h1' h0)
h2texprBind1 fmaph1 f (H2TExprMedia m) =
  H2TExprMedia $ m >>= \m' -> case m' of
    H2TExprHole0 h0 -> undefined -- let H2TExprMedia h0' = f h0 in h0'
    H2TExprHole1 h1 ->
      undefined
--      return $ H2TExprHole1 $ fmaph1 (h2texprBind0 fmaph1 f) h1
    H2TExprLayer0 layer ->
      undefined
--      return $ H2TExprLayer0 $ h2texprBind0 fmaph1 f layer
    H2TExprLayer1 layer ->
      undefined
--      return $ H2TExprLayer1 $
--      h2texprBind0 fmaph1 (h2texprReturn0 . h2texprBind0 fmaph1 f)
--        layer
    H2TExprLayer2 layer ->
      undefined
--      return $ H2TExprLayer2 $
--      h2texprBind0 (h2texprBind0 fmaph1 . (h2texprReturn0 .))
--        (h2texprReturn0 . h2texprBind0 fmaph1 f)
--        layer
h2texprJoin1 ::
  (Monad m) =>
  (forall h0 h0'. (h0 -> h0') -> h1 h0 -> h1 h0') ->
  (H2TExpr m (H2TExpr m h1) (H2TExpr m h1 h0) ->
    H2TExpr m h1 (H2TExpr m h1 h0))
h2texprJoin1 fmaph1 (H2TExprMedia m) =
  H2TExprMedia $ m >>= \m' -> case m' of
    H2TExprHole0 h0 -> return $ H2TExprHole0 $ h0
    H2TExprHole1 (H2TExprMedia h1) -> h1 >>= \h1' -> case h1' of
      H2TExprHole0 h0 ->
        let H2TExprMedia m' = h2texprJoin1 fmaph1 h0 in m'
      H2TExprHole1 h1 ->
        return $ H2TExprHole1 $
        fmaph1 (h2texprBind0 fmaph1 $ h2texprJoin1 fmaph1) h1
    H2TExprLayer0 layer ->
      return $ H2TExprLayer0 $ h2texprJoin1 fmaph1 layer
    H2TExprLayer1 layer ->
      return $ H2TExprLayer1 $
      h2texprJoin1 fmaph1 $
      h2texprMap0 (h2texprMap0 fmaph1) (h2texprJoin1 fmaph1) layer
    H2TExprLayer2 layer ->
      return $ H2TExprLayer2 $
      h2texprMap0 (h2texprMap0 fmaph1) (h2texprJoin1 fmaph1) $
      h2texprJoin1 (h2texprMap0 fmaph1) layer


-- TODO: This is an attempt to refactor the `H0Monad` family to factor
-- out an `H0Functor` family and an `H0Applicative` family, but we
-- haven't finished yet.
--
class H0Functor m0 where
  h0fmap :: (a -> a') -> m0 a -> m0 a'
class H1Functor m1 where
  h1fmap ::
--    (H0Functor m0, H0Functor m0') =>
    (forall a. m0 a -> m0' a) -> m1 m0 a -> m1 m0' a
class H2Functor m2 where
  h2fmap ::
--    (H1Functor m1, H1Functor m1', H0Functor m0) =>
    (forall m0 a. m1 m0 a -> m1' m0 a) -> m2 m1 m0 a -> m2 m1' m0 a
class (H0Functor m0) => H0Applicative m0 where
  h0fmapUnit :: a -> m0 a
  h0fmapPair ::
    (a -> b -> c) ->
    (m0 a -> m0 b -> m0 c)
class (H1Functor m1) => H1Applicative m1 where
  h1fmapUnit :: m0 a -> m1 m0 a
  h1fmapPair ::
--    (H1Functor m0a, H1Functor m0b, H1Functor m0c) =>
    (forall a. m0a a -> m0b a -> m0c a) ->
    (m1 m0a a -> m1 m0b a -> m1 m0c a)
class (H2Functor m2) => H2Applicative m2 where
  h2fmapUnit :: m1 m0 a -> m2 m1 m0 a
  h2fmapPair ::
--    (H1Functor m1a, H1Functor m1b, H1Functor m1c, H0Functor m0) =>
    (forall m0 a. m1a m0 a -> m1b m0 a -> m1c m0 a) ->
    (m2 m1a m0 a -> m2 m1b m0 a -> m2 m1c m0 a)
-- TODO: The definition of `H0Monoid` is different than the others in
-- the `H0Monoid` family. Figure out if that's a problem. Or,
-- actually, since the `H0Operad` family starts counting from
-- `H1Monoid`, just drop `H0Monoid` and call this family `H0Monad`
-- once it's ready to take the place of the `H0Monad` family we're
-- using now.
class H0Monoid w where
  h0mempty :: w
  h0mappend :: w -> w -> w
class H1Monoid w where
  h1mempty :: a -> w a
  h1mappend :: w (w a) -> w a
class H2Monoid w where
  h2mempty :: m0 a -> w m0 a
  h2mappend :: w (w m0) a -> w m0 a
class H3Monoid w where
  h3mempty :: m1 m0 a -> w m1 m0 a
  h3mappend :: w (w m1) m0 a -> w m1 m0 a
-- TODO: Are these even really operads? Anyway, the reason we defined
-- these was as an attempt to extrapolate a different kind of
-- `H0Applicative` family. Something we didn't account for in defining
-- this `H0Operad` family was the fact that the `h0fmapPair` family
-- can accept values of two different types, and our monoid families
-- here are homogeneous.
class (H1Monoid w) => H0Operad w m0 where
  h0operad :: (w a -> a) -> w (m0 a) -> m0 a
class (H2Monoid w) => H1Operad w m1 where
  h1operad :: (w m0 a -> m0 a) -> w (m1 m0) a -> m1 m0 a
class (H3Monoid w) => H2Operad w m2 where
  h2operad :: (w m1 m0 a -> m1 m0 a) -> w (m2 m1) m0 a -> m2 m1 m0 a
data H0FreeMonoid a = H0Nil | H0Cons a (H0FreeMonoid a)
instance H0Monoid (H0FreeMonoid a) where
  h0mempty = H0Nil
  h0mappend xs ys = case xs of
    H0Nil -> ys
    H0Cons x xs' -> H0Cons x $ h0mappend xs' ys
data H1FreeMonoid m0 a = H1Nil a | H1Cons (m0 (H1FreeMonoid m0 a))
instance (H0Functor m0) => H1Monoid (H1FreeMonoid m0) where
  h1mempty = H1Nil
  h1mappend m = case m of
    H1Nil a -> a
    H1Cons m' -> H1Cons $ h0fmap h1mappend m'
data H2FreeMonoid m1 m0 a =
  H2Nil (m0 a) | H2Cons (m1 (H2FreeMonoid m1 m0) a)
instance (H1Functor m1) => H2Monoid (H2FreeMonoid m1) where
  h2mempty = H2Nil
  h2mappend m = case m of
    H2Nil a -> a
    H2Cons m' -> H2Cons $ h1fmap h2mappend m'

makeConcreteH0FreeMonoid :: [a] -> H0FreeMonoid a
makeConcreteH0FreeMonoid xs = case xs of
  [] -> H0Nil
  x:xs' -> H0Cons x $ makeConcreteH0FreeMonoid xs'
makeConcreteH1FreeMonoid :: ([a], b) -> H1FreeMonoid ((,) a) b
makeConcreteH1FreeMonoid (xs, b) = case xs of
  [] -> H1Nil b
  x:xs' -> H1Cons $ (,) x $ makeConcreteH1FreeMonoid (xs', b)
-- TODO: Hmm, the type ([a], b, c) seems underwhelming. Maybe
-- `H2FreeMonoid` isn't worth much the way we've defined it. Maybe it
-- isn't even free for `H2Monoid`.
makeConcreteH2FreeMonoid ::
  ([a], b, c) -> H2FreeMonoid (Compose ((,) a)) ((,) b) c
makeConcreteH2FreeMonoid (xs, b, c) = case xs of
  [] -> H2Nil (b, c)
  x:xs' ->
    H2Cons $ Compose $ (,) x $ makeConcreteH2FreeMonoid (xs', b, c)
