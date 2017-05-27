-- macros.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.


{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (ap, join, liftM)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.Void (Void)



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
-- `nudgeNestQQ`.

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

data EnvEsc esc f lit
  = EnvEscErr String
  | EnvEscLit lit
  | forall esc' fa lita fb litb eof. EnvEscSubexpr
      esc
      (Env esc' fa lita fb litb)
      (QQExpr fa (EnvEsc (Maybe Void) fa (lita eof)))
      (QQExpr fb (litb eof) -> QQExpr f (EnvEsc esc f lit))

newtype Env esc fa lita fb litb = Env {
  callEnv ::
    forall eof.
    Env esc fa lita fb litb ->
    QQExpr fa (lita eof) ->
      Maybe (QQExpr fb (EnvEsc esc fb (litb eof)))
}

instance (Functor f) => Monad (EnvEsc esc f) where
  return = EnvEscLit
  effects >>= func = case effects of
    EnvEscErr err -> EnvEscErr err
    EnvEscLit lit -> func lit
    EnvEscSubexpr esc' env expr callback ->
      EnvEscSubexpr esc' env expr (fmap (>>= func) . callback)
instance (Functor f) => Applicative (EnvEsc esc f) where
  pure = return
  (<*>) = ap
instance (Functor f) => Functor (EnvEsc esc f) where
  fmap = liftM

processEsc ::
  (Functor f) =>
  (forall esc' fa lita fb litb eof'.
    esc ->
    Env esc' fa lita fb litb ->
    QQExpr fa (EnvEsc (Maybe Void) fa (lita eof')) ->
    (QQExpr fb (litb eof') -> QQExpr f (EnvEsc esc'' f (lit eof))) ->
    EnvEsc esc'' f (lit eof)) ->
  EnvEsc esc f (lit eof) ->
    EnvEsc esc'' f (lit eof)
processEsc onSubexpr esc = case esc of
  EnvEscErr err -> EnvEscErr err
  EnvEscLit lit -> EnvEscLit lit
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
-- includes (EnvEsc (Maybe Void) ...) so that it can use a `Nothing`
-- escape to trampoline to the macroexpander like this.
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
  Env esc fa lita fb litb ->
  QQExpr fa (lita eof) ->
    Maybe (QQExpr fb (EnvEsc esc fb (litb eof)))
interpret env expr = callEnv env env expr

callSimpleEnv ::
  (forall eof.
    Env esc fa lita fb litb ->
    fa (QQExpr fa (lita eof)) ->
      Maybe (QQExpr fb (EnvEsc esc fb (litb eof)))
  ) ->
  Env esc fa lita fb litb ->
  QQExpr fa (lita eof) ->
    Maybe (QQExpr fb (EnvEsc esc fb (litb eof)))
callSimpleEnv func env expr = case expr of
  QQExprLiteral lit -> Nothing
  QQExprQQ body -> Nothing
  QQExprList list -> func env list

simpleEnv ::
  (forall eof.
    Env esc fa lita fb litb ->
    fa (QQExpr fa (lita eof)) ->
      Maybe (QQExpr fb (EnvEsc esc fb (litb eof)))
  ) ->
    Env esc fa lita fb litb
simpleEnv func = Env $ callSimpleEnv func

shadowSimpleEnv ::
  (Functor fb) =>
  Env esc fa lita fb litb ->
  (forall eof.
    Env esc fa lita fb litb ->
    fa (QQExpr fa (lita eof)) ->
      Maybe (QQExpr fb (EnvEsc esc fb (litb eof)))
  ) ->
    Env esc fa lita fb litb
shadowSimpleEnv env shadower = Env $ \lateEnv expr ->
  case callSimpleEnv shadower lateEnv expr of
    Just result -> Just result
    Nothing -> callEnv env lateEnv expr

qualifyEnv ::
  (Functor fb) =>
  (Env esc'' fa lita fb litb -> Env esc fa lita fb litb) ->
  (forall esc' fa' lita' fb' litb' eof' eof.
    esc ->
    Env esc' fa' lita' fb' litb' ->
    QQExpr fa' (EnvEsc (Maybe Void) fa' (lita' eof')) ->
    (QQExpr fb' (litb' eof') ->
      QQExpr fb (EnvEsc esc'' fb (litb eof))) ->
    EnvEsc esc'' fb (litb eof)) ->
  Env esc fa lita fb litb ->
    Env esc'' fa lita fb litb
qualifyEnv super qualify env = Env $ \lateEnv expr ->
  fmap (fmap $ processEsc qualify) $ callEnv env (super lateEnv) expr

downEscEnv ::
  (Functor fb) =>
  Env (Maybe (Maybe esc)) fa lita fb litb ->
    Env (Maybe esc) fa lita fb litb
downEscEnv = qualifyEnv upEscEnv $ \esc env expr func -> case esc of
  -- If we have an escape that's supposed to be trampolined to the
  -- macroexpander (Nothing), it's still supposed to be trampolined.
  Nothing -> EnvEscSubexpr Nothing env expr func
  -- If we have an escape that's a closing paren (Just Nothing),
  -- trampoline the following syntax to the macroexpander (Nothing).
  Just esc' -> EnvEscSubexpr esc' env expr func

upEscEnv ::
  (Functor fb) =>
  Env (Maybe esc) fa lita fb litb ->
    Env (Maybe (Maybe esc)) fa lita fb litb
upEscEnv = qualifyEnv downEscEnv $ \esc env expr func -> case esc of
  -- If we have an escape that's supposed to be trampolined to the
  -- macroexpander (Nothing), it's still supposed to be trampolined.
  Nothing -> EnvEscSubexpr Nothing env expr func
  -- Otherwise, we at least know it's not a closing paren
  -- (Just Nothing).
  Just esc' -> EnvEscSubexpr (Just (Just esc')) env expr func

-- A good way to read this signature is to see that the operation
-- takes an interval enclosed by the parens (`QQExpr fb (...)`), some
-- following syntax for which that bracketed section could itself be
-- an opening bracket (`EnvEsc (Maybe esc) (OpParen fa fb) (...)`),
-- and finally a past-the-end-of-the-file section which the operator
-- must ignore (`lita`). The operator returns a possible bracketed
-- section which may trampoline to the macroexpander
-- (`QQExpr fb (EnvEsc (Maybe Void) fb (...))`) followed by some
-- syntax which has not been consumed
-- (`EnvEsc (Maybe esc) (OpParen fa fb) (...)`) followed by an
-- untouched past-the-end-of-the-file section (`lita`).
newtype OpParenOp fa fb = OpParenOp
  (forall esc lita.
    (Env (Maybe Void) (OpParen fa fb) Identity fb
      (EnvEsc (Maybe esc) (OpParen fa fb))) ->
    QQExpr fb (EnvEsc (Maybe esc) (OpParen fa fb) lita) ->
      QQExpr fb
        (EnvEsc (Maybe Void) fb
          (EnvEsc (Maybe esc) (OpParen fa fb) lita)))

data OpParen fa fb rest
  = OpParenOpen (OpParenOp fa fb) rest
  | OpParenClose rest
  | OpParenOther (fa rest)

-- TODO: Implement this if we can. We probably want to change this to
-- take an operator that acts on syntax in the next-higher level of
-- quasiquotation. Would that syntax really look anything like
-- `QQExpr`? What it might look like is more like this:
--
-- type SMap a = Map [String] a
--
-- SMap (forall lit. SMap (QQExpr g lit) -> QQExpr g lit) ->
--   (forall lit. SMap (QQExpr g lit) -> QQExpr g lit)
--
-- We can't just replace (OpParen fa fb rest) with
-- (OpParen (SMap ... -> ...) fb rest) becase we need to replace `fa`
-- with a functor, not a type.
--
opParenOpenQuasi ::
  forall fa fb rest. (Functor fa, Functor fb) =>
  (forall rest. rest -> OpParen fa fb rest) ->
  rest ->
    OpParen fa fb rest
opParenOpenQuasi op rest = flip OpParenOpen rest $ OpParenOp $
  \env expr -> undefined

opParenOpenEmpty ::
  forall fa fb rest. (Functor fa, Functor fb) =>
  (forall rest. rest -> OpParen fa fb rest) ->
  rest ->
    OpParen fa fb rest
opParenOpenEmpty op rest = flip OpParenOpen rest $ OpParenOp $
  \env expr -> QQExprLiteral $ case expr of
    QQExprLiteral esc -> case esc of
      EnvEscErr err -> EnvEscErr err
      EnvEscLit lit -> nonemptyErr
      EnvEscSubexpr esc' env' expr' func -> case esc' of
        Just esc'' -> nonemptyErr
        Nothing ->
          EnvEscSubexpr Nothing env' expr' $ \expr'' ->
          QQExprLiteral $
            EnvEscSubexpr Nothing env
              (fmap (fmap Identity . putOff) $ QQExprList $
                op $ func expr'') $
            fmap
              ((EnvEscLit ::
                 forall esc lita.
                 EnvEsc (Maybe esc) (OpParen fa fb) lita ->
                   EnvEsc (Maybe Void) fb
                     (EnvEsc (Maybe esc) (OpParen fa fb) lita)
              ) . join)
    _ -> nonemptyErr
  where
  
  nonemptyErr :: forall esc f lit. EnvEsc esc f lit
  nonemptyErr =
    EnvEscErr "Encountered a paren which held content within the paren itself"
  
  putOff ::
    forall esc f lit. (Functor f) =>
    EnvEsc (Maybe esc) f lit ->
      EnvEsc (Maybe Void) f (EnvEsc (Maybe esc) f lit)
  putOff esc = case esc of
    EnvEscErr err -> EnvEscErr err
    EnvEscLit lit -> EnvEscLit $ EnvEscLit lit
    EnvEscSubexpr esc' env expr func -> case esc' of
      Just esc'' ->
        EnvEscLit $ EnvEscSubexpr (Just esc'') env expr func
      Nothing -> EnvEscSubexpr Nothing env expr (fmap putOff . func)

-- These are four operators which interpret an `OpParenOpen` usage of
-- the form "(...bracketedBody...)...followingBody...": Two that
-- quasiquote and unquote the `bracketedBody`, and two that verify
-- `bracketedBody` is empty and then behave like
-- "(...followingBody..." or ")...followingBody...".
--
-- TODO: Complete the implementation of `opParenOpenQuasi`, which the
-- first two of these depend on.
--
opParenOpenQuasiquote ::
  (Functor fa, Functor fb) =>
  OpParenOp fa fb -> rest -> OpParen fa fb rest
opParenOpenQuasiquote op = opParenOpenQuasi $ OpParenOpen op
opParenOpenUnquote ::
  (Functor fa, Functor fb) => rest -> OpParen fa fb rest
opParenOpenUnquote = opParenOpenQuasi $ OpParenClose
opParenOpenEmptyOpen ::
  (Functor fa, Functor fb) =>
  OpParenOp fa fb -> rest -> OpParen fa fb rest
opParenOpenEmptyOpen op = opParenOpenEmpty $ OpParenOpen op
opParenOpenEmptyClose ::
  (Functor fa, Functor fb) => rest -> OpParen fa fb rest
opParenOpenEmptyClose = opParenOpenEmpty OpParenClose

instance (Functor fa) => Functor (OpParen fa ga) where
  fmap func x = case x of
    OpParenOpen parenFunc rest -> OpParenOpen parenFunc $ func rest
    OpParenClose rest -> OpParenClose $ func rest
    OpParenOther other -> OpParenOther $ fmap func other

-- NOTE: This environment expects at least one escape, namely
-- `Nothing`, with the meaning of trampolining to the macroexpander.
coreEnv ::
  (Functor fa, Functor fb) =>
  Env (Maybe Void) (OpParen fa fb) Identity fb
    (EnvEsc (Maybe esc) (OpParen fa fb))
coreEnv = simpleEnv coreEnv_

coreEnv_ ::
  -- We use an explicit `forall` here so we can use these type
  -- variables in type annotations below (with the help of
  -- `ScopedTypeVariables`). We're only using the type annotations as
  -- a sanity check.
  forall esc fa fb eof. (Functor fa, Functor fb) =>
  Env (Maybe Void) (OpParen fa fb) Identity fb
    (EnvEsc (Maybe esc) (OpParen fa fb)) ->
  OpParen fa fb (QQExpr (OpParen fa fb) (Identity eof)) ->
    Maybe
      (QQExpr fb
        (EnvEsc (Maybe Void) fb
          (EnvEsc (Maybe esc) (OpParen fa fb) eof)))
coreEnv_ env call = case call of
  OpParenOpen (OpParenOp op) expr -> Just $ QQExprLiteral $
    ((EnvEscSubexpr Nothing
      ((downEscEnv $ shadowSimpleEnv (upEscEnv env) $ \env' call'' ->
        case call'' of
          OpParenClose expr' -> Just $ QQExprLiteral $
            -- We reset the environment to the environment that
            -- existed at the open paren.
            EnvEscSubexpr (Just Nothing) (upEscEnv env)
              (fmap
                (EnvEscLit ::
                  forall lita.
                  Identity lita ->
                    EnvEsc (Maybe Void) (OpParen fa fb)
                      (Identity lita))
                expr')
              (fmap
                (EnvEscLit ::
                  forall lita.
                  (EnvEsc (Maybe esc) (OpParen fa fb) lita) ->
                  EnvEsc (Maybe (Maybe Void)) fb
                    (EnvEsc (Maybe esc) (OpParen fa fb) lita)))
          _ -> Nothing
      ) ::
        Env (Maybe Void) (OpParen fa fb) Identity fb
          (EnvEsc (Maybe esc) (OpParen fa fb)))
      (fmap
        (EnvEscLit ::
          Identity eof ->
            EnvEsc (Maybe Void) (OpParen fa fb) (Identity eof))
        expr
        :: QQExpr (OpParen fa fb)
             (EnvEsc (Maybe Void) (OpParen fa fb) (Identity eof)))
      ((\expr' ->
          -- We consult an operator determined by the matched opening
          -- and closing parens to determine what to do next. In this
          -- proof of concept, we determine the operation fully in
          -- terms of the opening paren.
          op env expr'
      ) ::
        QQExpr fb (EnvEsc (Maybe esc) (OpParen fa fb) eof) ->
          QQExpr fb
            (EnvEsc (Maybe Void) fb
              (EnvEsc (Maybe esc) (OpParen fa fb) eof)))
    ) :: EnvEsc (Maybe Void) fb
           (EnvEsc (Maybe esc) (OpParen fa fb) eof))
  OpParenClose expr ->
    Just $ QQExprLiteral $
      EnvEscErr "Encountered an unmatched closing paren"
  _ -> Nothing
