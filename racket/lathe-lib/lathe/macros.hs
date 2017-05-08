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
  x:xs -> Just $ Expr x $ fmap exprFromAny $ exprFromList xs

listFromExpr :: Maybe (Expr (Expr (Const Void)) a) -> [a]
listFromExpr expr = case expr of
  Nothing -> []
  Just (Expr x xs) -> x : listFromExpr (fmap anyFromExpr xs)

data SExpr a = SExpr a [SExpr a]
  deriving (Show)

exprFromSExpr :: SExpr a -> Expr (Expr (Expr (Const Void))) a
exprFromSExpr (SExpr a list) =
  Expr a $ exprFromList $ fmap exprFromSExpr list

sExprFromExpr :: Expr (Expr (Expr (Const Void))) a -> SExpr a
sExprFromExpr (Expr a expr) =
  SExpr a $ fmap sExprFromExpr $ listFromExpr expr

data Paren = ParenOpen | ParenClose

matchParens1 :: [Paren] -> ([SExpr ()], Maybe String)
matchParens1 list = case list of
  [] -> ([], Nothing)
  paren:parens -> case paren of
    ParenClose -> ([], Just "Encountered an unmatched ParenClose")
    ParenOpen -> case parseParenOpen parens of
      Left error -> ([], Just error)
      Right (result, rest) ->
        let (results, error) = matchParens1 rest in
        (result:results, error)
    where
      parseParenOpen :: [Paren] -> Either String (SExpr (), [Paren])
      parseParenOpen list = case list of
        [] -> Left "Encountered an unmatched ParenOpen"
        paren:parens -> case paren of
          ParenClose -> Right (SExpr () [], parens)
          ParenOpen ->
            parseParenOpen parens >>= \(subexpr, rest) ->
            parseParenOpen rest >>= \(SExpr () subexprs, following) ->
            return (SExpr () $ subexpr:subexprs, following)

matchParens2 :: [Paren] -> ([SExpr ()], Maybe String)
matchParens2 list = case list of
  [] -> ([], Nothing)
  paren:parens -> case paren of
    ParenClose -> ([], Just "Encountered an unmatched ParenClose")
    ParenOpen ->
      let (result, rest) = parseParenOpen parens in
      case rest of
        Left error -> (result, Just error)
        Right rest' ->
          let (results, error) = matchParens2 rest' in
          (SExpr () result : results, error)
    where
      parseParenOpen :: [Paren] -> ([SExpr ()], Either String [Paren])
      parseParenOpen list = case list of
        [] -> ([], Left "Encountered an unmatched ParenOpen")
        paren:parens -> case paren of
          ParenClose -> ([], Right parens)
          ParenOpen ->
            let (result, rest) = parseParenOpen parens in
            case rest of
              Left error -> ([], Left error)
              Right rest' ->
                let (results, rest'') = parseParenOpen rest' in
                (SExpr () result : results, rest'')

exprFromErrList ::
  ([a], Maybe String) ->
  Maybe
    (Either String (Expr (Const String `Sum` Expr (Const Void)) a))
exprFromErrList (list, error) = case list of
  [] -> case error of
    Nothing -> Nothing
    Just error' -> Just $ Left error'
  x:xs -> Just $ Right $ Expr x $ case exprFromErrList (xs, error) of
    Nothing -> Nothing
    Just xs' -> Just $ case xs' of
      Left error' -> InL $ Const error'
      Right xs' -> InR $ exprFromAny xs'

errListFromExpr ::
  Maybe
    (Either String (Expr (Const String `Sum` Expr (Const Void)) a)) ->
    ([a], Maybe String)
errListFromExpr expr = case expr of
  Nothing -> ([], Nothing)
  Just expr' -> case expr' of
    Left error -> ([], Just error)
    Right (Expr x body) -> case body of
      Nothing -> ([x], Nothing)
      Just body' -> case body' of
        InL (Const error) -> ([x], Just error)
        InR xs' ->
          let
            (xs, error) =
              errListFromExpr $ Just $ Right $ anyFromExpr xs'
          in
            (x:xs, error)

-- errListFromExpr $ exprFromErrList ([1, 2, 3, 4], Nothing)
-- errListFromExpr $ exprFromErrList ([1, 2, 3, 4], Just "woo")
-- errListFromExpr $ exprFromErrList ([1, 2], Nothing)
-- errListFromExpr $ exprFromErrList ([1, 2], Just "woo")
-- errListFromExpr $ exprFromErrList ([1], Nothing)
-- errListFromExpr $ exprFromErrList ([1], Just "woo")
-- errListFromExpr $ exprFromErrList ([], Nothing)
-- errListFromExpr $ exprFromErrList ([], Just "woo")
-- matchParens2 [ParenOpen, ParenOpen, ParenClose, ParenOpen, ParenClose, ParenClose, ParenOpen, ParenClose]


-- TODO: Wow, this is a swiss cheese of type errors right now. (See
-- all the undefined/TODO locations.) The places where `m` really
-- needs to be (Const Void) are beginning to show. It might be time to
-- think about what we're really trying to generalize this to, so that
-- we can start fresh with a better type signature in mind.
matchParens ::
  (Functor m) =>
  (forall a.
    (Expr (Expr (Expr m)) ()) ->
    Expr m (Expr (a `Sum` Expr m) (Expr (Expr (Expr m)) ())) ->
      Expr (a `Sum` Expr m) (Expr (Expr (Expr m)) ())) ->
  Maybe (Expr (Expr m) Paren) ->
  Maybe
    (Either String
      (Expr (Const String `Sum` Expr m) (Expr (Expr (Expr m)) ())))
matchParens prependTree expr = case expr of
  Nothing -> Nothing
  Just (Expr paren parens) -> case paren of
    ParenClose -> Just $ Left "Encountered an unmatched ParenClose"
    ParenOpen -> case parseParenOpen prependTree parens of
      Nothing -> Nothing
      Just result -> Just $ case result of
        Left errorOrEscape -> case errorOrEscape of
          Left error -> Left error
          -- TODO
          Right escape -> Right $ undefined  -- $ Expr (Expr () Nothing) $ Just $
--              processTopLevelEscape prependTree escape
        Right expr -> Right $ flip exprMapFunctor expr $ \rest ->
          case rest of
            InR expr' -> InR $ undefined expr'  -- TODO
            InL (Const errorOrEscape) -> case errorOrEscape of
              Left error -> InL (Const error)
              Right escape -> undefined $ processTopLevelEscape prependTree escape  -- TODO
    where
      
      processTopLevelEscape ::
        (Functor m) =>
        (forall a.
          (Expr (Expr (Expr m)) ()) ->
          Expr m (Expr (a `Sum` Expr m) (Expr (Expr (Expr m)) ())) ->
            Expr (a `Sum` Expr m) (Expr (Expr (Expr m)) ())) ->
        Expr (Expr m) Paren ->
          Sum (Const String) (Expr m) (Expr (Expr (Expr m)) ())
      processTopLevelEscape prependTree escape =
        case matchParens prependTree $ Just escape of
          Nothing ->
            error "Internal error: Received a `Nothing` result from `matchParens` for a non-`Nothing` input"
          Just escape' -> case escape' of
            Left error -> InL $ Const error
            Right rest -> InR $ undefined $ Expr rest Nothing  -- TODO
      
      parseParenOpen ::
        -- NOTE: We use an encoding trick here. What we want to
        -- represent is a generalization of
        -- ([SExpr ()], Either String [Paren]) like so:
        --
        --   (SExpr ()) generalizes to (Expr (Expr (Expr m)) ())
        --   [Paren] generalizes to (Maybe (Expr (Expr m) Paren))
        --   ([a], Either String b) generalizes to ???
        --
        -- That last one is tough. Fortunately, we know a similar
        -- representation based on
        -- `exprFromErrList`/`errListFromExpr`:
        --
        --   ([a], Maybe b)
        --     generalizes to
        --     (Maybe (Either b (Expr (Const b `Sum` Expr m) a)))
        --
        -- Since our generalization of [Paren] actually begins with
        -- (Maybe ...), instead of generalizing
        --
        --   ([SExpr ()], Either String (Maybe (Expr (Expr m) Paren)))
        --
        -- we can actually generalize this:
        --
        --   ([SExpr ()], Maybe (Either String (Expr (Expr m) Paren)))
        --
        -- That's what we do here.
        --
        (forall a.
          (Expr (Expr (Expr m)) ()) ->
          Expr m (Expr (a `Sum` Expr m) (Expr (Expr (Expr m)) ())) ->
            Expr (a `Sum` Expr m) (Expr (Expr (Expr m)) ())) ->
        Maybe (Expr m (Expr (Expr m) Paren)) ->
          Maybe
            (Either (Either String (Expr (Expr m) Paren))
              (Expr
                (Sum (Const (Either String (Expr (Expr m) Paren)))
                  (Expr m))
                (Expr (Expr (Expr m)) ())))
      parseParenOpen prependTree list = case list of
        Nothing ->
          Just $ Left $ Left "Encountered an unmatched ParenOpen"
        -- TODO: The `parseParenOpen` function originally writen for a
        -- `list` parameter of type (Maybe (Expr (Expr m) Paren)), and
        -- we haven't updated it yet. This seems like another case
        -- (aside from `prependTree`) where if only `m` were
        -- (Const Void), we would have a straightforward
        -- implementation here.
        Just (Expr paren parens) -> case undefined paren of  -- TODO
          ParenClose -> case parens of
            -- The parse escapes with an empty list.
            Nothing -> Nothing
            -- The parse escapes with a nonempty list.
            Just parens' -> Just $ Left $ Right $ undefined parens'  -- TODO
          ParenOpen -> Just $ case parseParenOpen prependTree $ undefined parens of  -- TODO
            -- A close paren caused the parse to escape with an empty
            -- list remaining and no s-expressions in between, but
            -- this open paren catches it and adds an empty
            -- s-expression.
            Nothing -> Right $ Expr (Expr () Nothing) Nothing
            Just resultAndRest -> case resultAndRest of
              -- The parse escapes with an immediate error or a
              -- nonempty list.
              Left errorOrEscape -> case errorOrEscape of
                -- The parse escapes with an error.
                Left error -> Left $ Left error
                -- A close paren caused the parse to escape with a
                -- nonempty list remaining and no s-expressions in
                -- between, but this open paren catches it, parses the
                -- remaining list (letting any escapes through this
                -- time), and adds an empty s-expression.
                Right rest ->
                  Right $ parseParenOpenCons (undefined prependTree)  -- TODO
                    (Expr () Nothing)
                    (undefined rest)  -- TODO
              Right (Expr result rest) -> case undefined rest of  -- TODO
                -- A close paren caused the parse to escape with an
                -- empty list remaining and one s-expression in
                -- between, but this open paren catches it and
                -- nests that s-expression under another s-expression
                -- layer.
                Nothing ->
                  Right $
                    Expr (Expr () $ Just $ Expr result Nothing)
                      Nothing
                Just rest' -> case rest' of
                  InL (Const errorOrEscape) -> case errorOrEscape of
                    -- The parse escapes with an error, discarding any
                    -- content that was parsed between the open paren
                    -- and that error.
                    Left error -> Left $ Left error
                    -- A close paren caused the parse to escape with a
                    -- nonempty list remaining and one s-expression in
                    -- between, but this open paren catches it, parses
                    -- the remaining list (letting any escapes through
                    -- this time), and nests that s-expression under
                    -- another s-expression layer.
                    Right rest ->
                      Right $ parseParenOpenCons prependTree
                        (Expr () $ Just $ Expr result Nothing)
                        rest
                  InR rest'' -> Right $
                    -- The type of `rest''` is
                    -- (Expr m
                    --   (Expr (Sum ...) (Expr (Expr (Expr m)) ()))),
                    -- and the type we want to return is
                    -- (Expr (Sum ...) (Expr (Expr (Expr m)) ())).
                    -- In the case we're generalizing from, `m` is
                    -- (Const Void), so (Expr m) is equivalent to
                    -- `Identity`, and all we would do is prepend
                    -- `result` as an element into the first layer of
                    -- the first s-expresion
                    -- (Expr (Expr (Expr m)) ())). But since (Expr m)
                    -- may be more generalized now, what we want to
                    -- do is apply a general operation to "prepend" it
                    -- onto all those lists at once. So we take this
                    -- operation as a parameter.
                    --
                    -- TODO: See if we can do something to help
                    -- *implement* this kind of parameter.
                    --
                    prependTree result rest''
      
      parseParenOpenCons ::
        (forall a.
          (Expr (Expr (Expr m)) ()) ->
          Expr m (Expr (a `Sum` Expr m) (Expr (Expr (Expr m)) ())) ->
            Expr (a `Sum` Expr m) (Expr (Expr (Expr m)) ())) ->
        Expr (Expr (Expr m)) () ->
        Expr m (Expr (Expr m) Paren) ->
          (Expr
            (Const (Either String (Expr (Expr m) Paren)) `Sum` Expr m)
            (Expr (Expr (Expr m)) ()))
      parseParenOpenCons prependTree op body =
        Expr op $ case parseParenOpen prependTree $ Just body of
          Nothing -> Nothing
          Just resultAndRest -> Just $ case resultAndRest of
            Left errorOrEscape -> InL $ Const errorOrEscape
            Right expr -> InR $ Expr expr Nothing

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

-- TODO: This commented-out type signature is pickier but more
-- symmetrical. See if we should go back to it.
--exprMapFunctor :: (Functor m, Functor n) =>
--  (forall a. m a -> n a) ->
--    (forall a. Expr m a -> Expr n a)
exprMapFunctor ::
  (Functor m, Functor n) =>
  (m (Expr m a) -> n (Expr m a)) -> Expr m a -> Expr n a
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
