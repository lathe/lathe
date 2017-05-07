-- macros.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.


{-# LANGUAGE RankNTypes, TypeOperators, FlexibleInstances #-}

import Data.Void
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Sum


data Expr m a = Expr a (Maybe (m (Expr m a)))

instance (Functor m) => Functor (Expr m) where
  fmap func (Expr a rest) =
    Expr (func a) $ fmap (fmap (fmap func)) rest

exprFromAny :: a -> Expr (Const Void) a
exprFromAny x = Expr x Nothing

anyFromExpr :: Expr (Const Void) a -> a
anyFromExpr (Expr result body) = case body of
  Nothing -> result
  Just body' -> absurd $ getConst body'


exprFromList :: [a] -> Maybe (Expr (Expr (Const Void)) a)
exprFromList list = case list of
  [] -> Nothing
  x:xs -> Just $ Expr x $ fmap exprFromAny (exprFromList xs)

listFromExpr :: Maybe (Expr (Expr (Const Void)) a) -> [a]
listFromExpr expr = case expr of
  Nothing -> []
  Just (Expr x xs) -> x : listFromExpr (fmap anyFromExpr xs)

data SExpr a = SExpr a [SExpr a]

exprFromSExpr :: SExpr a -> Expr (Expr (Expr (Const Void))) a
exprFromSExpr (SExpr a list) =
  Expr a $ exprFromList $ fmap exprFromSExpr list

sExprFromExpr :: Expr (Expr (Expr (Const Void))) a -> SExpr a
sExprFromExpr (Expr a expr) =
  SExpr a $ fmap sExprFromExpr $ listFromExpr expr


data Env esc err m a b =
  Env (Env esc err m a b -> Expr m a ->
    Expr (Const (esc, Expr m a) `Sum` m) (Either err b))

data CoreOp = OpId

class (Functor m) => SyntaxMaterial m where
  getOneAndOnly :: m a -> Maybe a

-- NOTE: There's a lot going on in the result type. The (Either err b)
-- part allows syntax errors to be reported locally in the result as
-- (Left ...) operators. The (esc, ...) option allows us to represent
-- the result of parsing a closing parenthesis or an unquote
-- operation.
interpret ::
  (Functor m) =>
  Env esc err m a b -> Expr m a ->
    Expr (Const (esc, Expr m a) `Sum` m) (Either err b)
interpret env@(Env envFunc) expr = envFunc env expr

-- TODO: See if we'll use this.
exprMapFunctor :: (Functor m, Functor n) =>
  (forall a. m a -> n a) ->
    (forall a. Expr m a -> Expr n a)
exprMapFunctor func (Expr op body) =
  Expr op $ fmap (fmap (exprMapFunctor func) . func) body


coreEnv ::
  (SyntaxMaterial m) =>
  Env esc String m (Either CoreOp a) a ->
  Expr m (Either CoreOp a) ->
    Expr (Const (esc, Expr m (Either CoreOp a)) `Sum` m)
      (Either String a)
coreEnv env (Expr op body) = case op of
  -- TODO: This potentially loops forever if `env` is (Env coreEnv).
  -- See if we can make an unrecognized operator result in an error
  -- message instead.
  Right op' -> interpret env (Expr (Right op') body)
  Left op' -> case op' of
    OpId -> flip Expr Nothing $ case body of
      Nothing -> Left "Called OpId with no arguments"
      Just body' -> case getOneAndOnly body' of
        Nothing ->
          Left "Called OpId with a number of arguments other than one"
        -- The idea of `coreInterpret` is that it's okay to make calls
        -- to whitespace-like operations like `OpId` itself inside the
        -- `OpId` region, but any other content doesn't make sense
        -- there.
        Just body'' -> coreInterpret body''

coreInterpret ::
  (SyntaxMaterial m) => Expr m (Either CoreOp a) -> Either String a
coreInterpret expr =
  let (Expr op body) = interpret (Env coreEnv) expr in
  case op of
    Left err -> Left err
    Right result -> case body of
      Nothing -> Right result
      Just body' -> case body' of
        InL (Const (esc, Expr op'' body'')) -> absurd esc
        InR body'' ->
          Left "Internal error: Got a non-`Nothing` monad from a result of evaluating with the core environment"

instance (SyntaxMaterial m) =>
  SyntaxMaterial (Compose (Expr m) (Sum (Const CoreOp) Identity))
  where
    getOneAndOnly =
      either (const Nothing) Just .
      coreInterpret .
      fmap (\sum -> case sum of
        InL (Const op') -> Left op'
        InR (Identity op') -> Right op'
      ) .
      getCompose

instance (Functor m) => SyntaxMaterial (Expr m) where
  getOneAndOnly (Expr result body) = case body of
    Nothing -> Just result
    Just body' -> Nothing

instance SyntaxMaterial Maybe where
  getOneAndOnly = id

-- TODO: Update the following old notes to explain the `Expr` data
-- type we define above.

-- We've defined encoders and parsers for traditional lists and
-- s-expressions, but they involve several potential error conditions,
-- and they're not very consistent with each other, which is going to
-- make it difficult to extrapolate to higher orders of
-- quasiquotation.
--
-- Ideally there's only one error condition we need: These lists and
-- s-expressions are distinguished by having a designated limit to the
-- degree of quasiquotation they can do, so we want to worry about
-- that error and no others.
--
-- We define `CallSExpr`, an s-expression representation which can be
-- parsed from a particular variant of `Expr` while having only that
-- potential error. The corresponding (nonempty) list representation
-- is `CallList`, and they're related by a functor `ExprAsCallFunctor`
-- which can be repeatedly applied to `Identity` to obtain higher
-- degrees of the syntax.
--
--
-- TODO: This actually suggests a drastic revision to `Expr`:
--
--   data Expr m a = Expr a (Maybe (m (Expr m a)))
--
-- That way the finite-order representations are (Const Void),
-- (Expr (Const Void)), (Expr (Expr (Const Void))), and so on, and the
-- infinite-order representation is (Fix Expr).
--
-- This almost corresponds to the formulation we have now, where there
-- are two layers of `Expr`:
--
--   from
--     x m a = a * (1 + (m (x m a)))
--     m = fix x
--   we derive
--     x (fix x) a = a * (1 + (fix x (x (fix x) a)))
--     fix x a = a * (1 + (fix x (fix x a)))
--     m a = a * (1 + (m (m a)))
--
-- When we use that kind of two-layer representation, we lose the
-- important ability to set a different initial value of `m`. Setting
-- a different `m` lets us apply this higher quasiquotation design
-- starting from different concrete syntaxes.
