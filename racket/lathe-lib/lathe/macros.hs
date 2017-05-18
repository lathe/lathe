-- macros.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.


{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Void (Void, absurd)
import Control.Monad (ap, liftM)



-- We aim to generalize the two concepts of balanced parentheses and
-- balanced quasiquote/unquote operations.
--
-- We define two main types, `SExpr` and `QQExpr`. Our goal with
-- `QQExpr` is to represent a data structure where quasiquote and
-- unquote are balanced by construction, while `SExpr` is the
-- corresponding freeform representation where the operations may or
-- may not be balanced.
--
-- Since we're generalizing, we simply refer to the generalized
-- quasiquotation and unquotation operations as opening and closing
-- parens, repsectively. Just as nested quasiquotations have more
-- nesting structure than nested lists, which have more nesting
-- structure than flat text, we'll be able to extrapolate upward to
-- even higher degrees of nesting.
--
-- The point of all this is so that we can design a macro system where
-- symbols like "quasiquote", "unquote", "(", and ")" are within the
-- realm of what users can define, and so that they're defined in ways
-- that are analogous to each other, leading to a language with fewer
-- hardcoded symbols and gotchas.

data SExpr f lit
  = SExprLiteral lit
  | SExprList (f (SExpr f lit))

data QQExpr f lit
  = QQExprLiteral lit
  | QQExprList (f (QQExpr f lit))
  | QQExprQQ (QQExpr f (QQExpr f lit))


data ParensAdded f a = ParenOpen a | ParenClose a | ParenOther (f a)


-- We define a few type class instances related to those types.

instance (Functor f) => Functor (ParensAdded f) where
  fmap func parensAdded = case parensAdded of
    ParenOpen rest -> ParenOpen $ func rest
    ParenClose rest -> ParenClose $ func rest
    ParenOther rest -> ParenOther $ fmap func rest

instance (Functor f) => Monad (SExpr f) where
  return = SExprLiteral
  effects >>= func = case effects of
    SExprLiteral lit -> func lit
    SExprList list -> SExprList $ fmap (>>= func) list
instance (Functor f) => Applicative (SExpr f) where
  pure = return
  (<*>) = ap
instance (Functor f) => Functor (SExpr f) where
  fmap = liftM

instance (Functor f) => Monad (QQExpr f) where
  return = QQExprLiteral
  effects >>= func = case effects of
    QQExprLiteral lit -> func lit
    QQExprList list -> QQExprList $ fmap (>>= func) list
    QQExprQQ body -> body >>= \unquote -> unquote >>= func
instance (Functor f) => Applicative (QQExpr f) where
  pure = return
  (<*>) = ap
instance (Functor f) => Functor (QQExpr f) where
  fmap = liftM


-- This is the algorithm that justifies the data types we're using: A
-- `QQExpr` corresponds to the special case of `SExpr` where all
-- parens within are balanced. This correspondence is witnessed by
-- the total function `flattenQQ` and its partial function inverse
-- `nudgeBestQQ`.

flattenQQ :: (Functor f) => QQExpr f lit -> SExpr (ParensAdded f) lit
flattenQQ qqExpr = case qqExpr of
  QQExprLiteral lit -> SExprLiteral lit
  QQExprList list -> SExprList $ ParenOther $ fmap flattenQQ list
  QQExprQQ body ->
    SExprList $ ParenOpen $ flattenQQ body >>=
    (SExprList . ParenClose . flattenQQ)

nudgeNestQQ ::
  (Functor f) =>
  SExpr (ParensAdded f) lit -> QQExpr f (Either String lit)
nudgeNestQQ sExpr = flip fmap (loop sExpr) $ \litOrEsc ->
  case litOrEsc of
    Left lit -> lit
    Right esc -> Left "Encountered an unmatched closing paren"
  where
    loop ::
      (Functor f) =>
      SExpr (ParensAdded f) lit ->
        QQExpr f
          (Either (Either String lit) (SExpr (ParensAdded f) lit))
    loop sExpr = case sExpr of
      SExprLiteral lit -> QQExprLiteral $ Left $ Right lit
      SExprList list -> case list of
        ParenOpen list' ->
          QQExprQQ $ flip fmap (loop list') $ \errOrLitOrEsc ->
            case errOrLitOrEsc of
              Left errOrLit -> QQExprLiteral $ Left $ Left $
                case errOrLit of
                  Left err -> err
                  Right lit ->
                    "Encountered an unmatched opening paren"
              Right esc -> loop esc
        ParenClose list' -> QQExprLiteral $ Right list'
        ParenOther list' -> QQExprList $ fmap loop list'

insistNestQQ ::
  (Functor f) => SExpr (ParensAdded f) lit -> QQExpr f lit
insistNestQQ sExpr = flip fmap (nudgeNestQQ sExpr) $ \errOrLit ->
  case errOrLit of
    Left err -> error err
    Right lit -> lit


-- Here's the obvious inclusion from `SExpr` into `QQExpr`, along with
-- its partial function inverse. Together with `flattenQQ` and
-- `nudgeNestQQ`, these functions mean we can nestle or flatten
-- something over and over. For instance, we could flatten a
-- quasiquote/unquote structure into an s-expression and then flatten
-- that s-expression into text.

qqExprOfSExpr :: (Functor f) => SExpr f lit -> QQExpr f lit
qqExprOfSExpr sExpr = case sExpr of
  SExprLiteral lit -> QQExprLiteral lit
  SExprList list -> QQExprList $ fmap qqExprOfSExpr list

nudgeSExprFromQQExpr ::
  (Functor f) => QQExpr f lit -> SExpr f (Maybe lit)
nudgeSExprFromQQExpr qqExpr = case qqExpr of
  QQExprLiteral lit -> SExprLiteral $ Just lit
  QQExprList list -> SExprList $ fmap nudgeSExprFromQQExpr list
  QQExprQQ body -> SExprLiteral Nothing

insistSExprFromQQExpr :: (Functor f) => QQExpr f lit -> SExpr f lit
insistSExprFromQQExpr qqExpr =
  nudgeSExprFromQQExpr qqExpr >>= \litOrErr -> case litOrErr of
    Nothing -> error "Tried to insistSExprFromQQExpr a QQExprQQ"
    Just lit -> return lit



-- The form of `SExpr` and `QQExpr` is the most familiar when
-- `SExprList` and `QQExprList` refer to actual lists (and hence
-- `SExpr` refers to actual s-expressions), but we can use other
-- functors for `f`.
--
-- If we use the functor (Compose [] (Sum (Const a) Identity)), then
-- our s-expressions can contain literal values of type `a`. We can
-- put symbols or other atoms into our s-exprssions that way, making
-- them even more familiar from the Lisp s-expression tradition.
--
-- If we use the functor ((,) a), then `SExpr` is isomorphic to a
-- list, and `QQExpr` is isomorphic to a list of s-expressions, as we
-- show using the coercions below. So the same algorithm we used for
-- flattening quasiquote/unquote can be used to flatten an
-- s-expression into a list with parens in it. So naming the
-- quasiquote/unquote operations "ParenOpen" and "ParenClosed" was
-- well justified.

improperListAsSExpr :: ([a], end) -> SExpr ((,) a) end
improperListAsSExpr (list, end) = case list of
  [] -> SExprLiteral end
  x:xs -> SExprList (x, improperListAsSExpr (xs, end))

deImproperListAsSExpr :: SExpr ((,) a) end -> ([a], end)
deImproperListAsSExpr expr = case expr of
  SExprLiteral end -> ([], end)
  SExprList (x, xs) ->
    let (xs', end) = deImproperListAsSExpr xs in (x:xs', end)

listAsSExpr :: [a] -> SExpr ((,) a) ()
listAsSExpr list = improperListAsSExpr (list, ())

deListAsSExpr :: SExpr ((,) a) () -> [a]
deListAsSExpr expr =
  let (list, ()) = deImproperListAsSExpr expr in list

improperNestedListsAsQQExpr ::
  ([SExpr [] a], end) -> QQExpr ((,) a) end
improperNestedListsAsQQExpr (nestedLists, end) = case nestedLists of
  [] -> QQExprLiteral end
  x:xs ->
    let xs' = improperNestedListsAsQQExpr (xs, end) in
    case x of
      SExprLiteral x' -> QQExprList (x', xs')
      SExprList list ->
        QQExprQQ $ improperNestedListsAsQQExpr (list, xs')

deImproperNestedListsAsQQExpr ::
  QQExpr ((,) a) end -> ([SExpr [] a], end)
deImproperNestedListsAsQQExpr qqExpr = case qqExpr of
  QQExprLiteral end -> ([], end)
  QQExprList (x, xs) ->
    let (xs', end) = deImproperNestedListsAsQQExpr xs in
    (SExprLiteral x : xs', end)
  QQExprQQ body ->
    let (list, unquote) = deImproperNestedListsAsQQExpr body in
    let (xs, end) = deImproperNestedListsAsQQExpr unquote in
    (SExprList list : xs, end)

nestedListsAsQQExpr :: [SExpr [] a] -> QQExpr ((,) a) ()
nestedListsAsQQExpr nestedLists =
  improperNestedListsAsQQExpr (nestedLists, ())

deNestedListsAsQQExpr :: QQExpr ((,) a) () -> [SExpr [] a]
deNestedListsAsQQExpr qqExpr =
  let (list, ()) = deImproperNestedListsAsQQExpr qqExpr in list



-- Equipped with a generalized notion of nested syntax, we demonstrate
-- a generalized macroexpansion technique where parentheses can be
-- defined as macros. (Or, if you prefer, this is an interpreter that
-- happens to return an expression.)

data EnvEsc esc f g
  = EnvEscErr String
  | forall esc' f' g'. EnvEscSubexpr
      esc
      (Env esc' f' g')
      (QQExpr f' (EnvEsc () f' g'))
      (QQExpr g' Void -> QQExpr g (EnvEsc esc f g))

newtype Env esc f g = Env {
  callEnv ::
    Env esc f g -> QQExpr f Void -> Maybe (QQExpr g (EnvEsc esc f g))
}

processEsc ::
  (Functor g) =>
  (forall esc' f' g'.
    esc ->
    Env esc' f' g' ->
    QQExpr f' (EnvEsc () f' g') ->
    (QQExpr g' Void -> QQExpr g (EnvEsc esc'' f g)) ->
    EnvEsc esc'' f g) ->
  EnvEsc esc f g ->
    EnvEsc esc'' f g
processEsc onSubexpr esc = case esc of
  EnvEscErr err -> EnvEscErr err
  EnvEscSubexpr esc' env expr func ->
    onSubexpr esc' env expr (fmap (processEsc onSubexpr) . func)

-- NOTE: There's a lot going on in the result type. The (Maybe ...)
-- part allows an environment to say that it has no binding for the
-- "operator" in this expression, rather than calling its provided
-- late-bound environment (eventually itself) over and over in an
-- infinite loop. The `EnvEscErr` result allows syntax errors to be
-- reported locally among the branches of the returned expression.
-- Finally, the `EnvEscSubexpr` result allows us to represent the
-- result of parsing a closing parenthesis/unquote or to invoke the
-- macroexpander in a trampolined way, so that we can potentially
-- cache and recompute different areas of the syntax without starting
-- from scratch each time. The expression parameter to `EnvEscSubexpr`
-- includes (EnvEsc () ...) so that it can use a () escape to
-- trampoline to the macroexpander like this.
--
-- TODO: We don't currently implement a cache like that. Should we? If
-- we could somehow guarantee that one macroexpansion step didnt't
-- peek at the internal structure of the `EnvEscSubexpr` values it
-- returned, we could have much more confidence in caching results. If
-- we start to formalize that, we'll likewise want to keep track of
-- what parts of the environment have been peeked at as well. We might
-- be able to make a little bit of headway on this if we use type
-- quantifiers, but it's likely this will be much more
-- straightoforward to enforce if we make peeking a side effect (so
-- we'll want to treat expressions not as pure data structures but as
-- imperative streams).
--
interpret ::
  Env esc f g -> QQExpr f Void -> Maybe (QQExpr g (EnvEsc esc f g))
interpret env expr = callEnv env env expr

callSimpleEnv ::
  (Env esc f g ->
    f (QQExpr f Void) ->
      Maybe (QQExpr g (EnvEsc esc f g))
  ) ->
  Env esc f g ->
  QQExpr f Void ->
    Maybe (QQExpr g (EnvEsc esc f g))
callSimpleEnv func env expr = case expr of
  QQExprLiteral lit -> Nothing
  QQExprQQ body -> Nothing
  QQExprList list -> func env list

simpleEnv ::
  (Env esc f g ->
    f (QQExpr f Void) ->
      Maybe (QQExpr g (EnvEsc esc f g))
  ) ->
    Env esc f g
simpleEnv func = Env $ callSimpleEnv func

shadowSimpleEnv ::
  (Functor g) =>
  Env esc f g ->
  (Env esc f g ->
    f (QQExpr f Void) ->
      Maybe (QQExpr g (EnvEsc esc f g))
  ) ->
    Env esc f g
shadowSimpleEnv env shadower = Env $ \lateEnv expr ->
  case callSimpleEnv shadower lateEnv expr of
    Just result -> Just result
    Nothing -> callEnv env lateEnv expr

qualifyEnv ::
  (Functor g) =>
  (Env esc'' f g -> Env esc f g) ->
  (forall esc' f' g'.
    esc ->
    Env esc' f' g' ->
    QQExpr f' (EnvEsc () f' g') ->
    (QQExpr g' Void -> QQExpr g (EnvEsc esc'' f g)) ->
    EnvEsc esc'' f g) ->
  Env esc f g ->
    Env esc'' f g
qualifyEnv super qualify env = Env $ \lateEnv expr ->
  fmap (fmap $ processEsc qualify) $ callEnv env (super lateEnv) expr

downEscEnv ::
  (Functor g) => Env (Maybe (Maybe esc)) f g -> Env (Maybe esc) f g
downEscEnv = qualifyEnv upEscEnv $ \esc env expr func -> case esc of
  -- If we have an escape that's supposed to be trampolined to the
  -- macroexpander (Nothing), it's still supposed to be trampolined.
  Nothing -> EnvEscSubexpr Nothing env expr func
  -- If we have an escape that's a closing paren (Just Nothing),
  -- trampoline the following syntax to the macroexpander (Nothing).
  Just esc' -> EnvEscSubexpr esc' env expr func

upEscEnv ::
  (Functor g) => Env (Maybe esc) f g -> Env (Maybe (Maybe esc)) f g
upEscEnv = qualifyEnv downEscEnv $ \esc env expr func -> case esc of
  -- If we have an escape that's supposed to be trampolined to the
  -- macroexpander (Nothing), it's still supposed to be trampolined.
  Nothing -> EnvEscSubexpr Nothing env expr func
  -- Otherwise, we at least know it's not a closing paren
  -- (Just Nothing).
  Just esc' -> EnvEscSubexpr (Just (Just esc')) env expr func

data OpParen f g a
  = OpParenOpen
      -- TODO: Figure out what kind of operation can actually be
      -- expressed with this signature. What this is supposed to
      -- represent is an operation that takes an interval enclosed by
      -- the parens (`QQExpr f`), some following syntax for which that
      -- bracketed section could itself be an opening bracket
      -- (`QQExpr (OpParen f g)`), and finally a section that's off
      -- limits which the operator must ignore (`lit`). The operator
      -- returns a possible parenthesis (`OpParen f g`) followed by
      -- instructions on how to parse whatever fragment following the
      -- parens has not been consumed (`EnvEsc () (OpParen f g) f`).
      --
      -- We probably need to at least upgrade this signature to accept
      -- an environment, so that it has some environment to refer to
      -- in an `EnvExcSubexpr` result.
      --
      -- We should try to represent four operators this way: Two that
      -- act as opening and closing parens of the next-higher
      -- quasiquotation degree for the bracketed body, and two that
      -- verify that the brackets contain nothing and then make the
      -- pair of brackets act as an opening or closing paren itself.
      --
      (forall lit.
        QQExpr g (QQExpr (OpParen f g) lit) ->
          OpParen f g (EnvEsc () (OpParen f g) g))
      a
  | OpParenClose a
  | OpParenOther (f a)

instance (Functor f) => Functor (OpParen f g) where
  fmap func x = case x of
    OpParenOpen parenFunc a -> OpParenOpen parenFunc $ func a
    OpParenClose a -> OpParenClose $ func a
    OpParenOther other -> OpParenOther $ fmap func other

-- NOTE: This environment expects at least one escape, namely
-- `Nothing`, with the meaning of trampolining to the macroexpander.
coreEnv ::
  -- We use an explicit `forall` here so we can use these type
  -- variables in type annotations below (with the help of
  -- `ScopedTypeVariables`). We're only using the type annotations as
  -- a sanity check.
  forall esc f g. (Functor f, Functor g) =>
  Env (Maybe esc) (OpParen f g) g
coreEnv = simpleEnv $ \env call -> case call of
  OpParenOpen parenFunc expr -> Just $ QQExprLiteral $
    ((EnvEscSubexpr Nothing
      ((downEscEnv $ shadowSimpleEnv (upEscEnv env) $ \env' call'' ->
        case call'' of
          OpParenClose expr' -> Just $ QQExprLiteral $
            -- We reset the environment to the environment that
            -- existed at the open paren.
            EnvEscSubexpr (Just Nothing) (upEscEnv env)
              (fmap absurd expr') (fmap absurd)
          _ -> Nothing
      ) :: Env (Maybe esc) (OpParen f g) g)
      (fmap absurd expr
        :: QQExpr (OpParen f g) (EnvEsc () (OpParen f g) g))
      ((\expr' ->
          -- We immediately run another interpretation pass over this
          -- result, using an operator determined from the parens
          -- (namely, `parenFunc`).
          QQExprLiteral $ EnvEscSubexpr Nothing env
            (QQExprList $ fmap QQExprLiteral $
              parenFunc $ fmap absurd expr')
            (fmap absurd)
      ) ::
        QQExpr g Void ->
          QQExpr g (EnvEsc (Maybe esc) (OpParen f g) g))
    ) :: EnvEsc (Maybe esc) (OpParen f g) g)
  OpParenClose expr ->
    Just $ QQExprLiteral $
      EnvEscErr "Encountered an unmatched closing paren"
  _ -> Nothing
