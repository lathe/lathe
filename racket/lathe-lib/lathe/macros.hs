-- macros.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.


{-# LANGUAGE RankNTypes #-}

import Data.Void
import Data.Functor.Identity
import Data.Functor.Compose


data Expr m a lit = Literal lit | Call a (Expr m a (m (Expr m a lit)))

data Env m esc err a b lit =
  Env (Env m esc err a b lit -> a -> Expr m a (m (Expr m a lit)) ->
    Expr m (Either err b) (Either (esc, Expr m a lit) lit))

data CoreOp = OpId

class (Functor m) => SyntaxMaterial m where
  getOneAndOnly :: m a -> Maybe a

callEnv ::
  (Functor m) =>
  Env m esc err a b lit -> a -> Expr m a (m (Expr m a lit)) ->
    Expr m (Either err b) (Either (esc, Expr m a lit) lit)
callEnv env@(Env envFunc) op expr = envFunc env op expr

-- NOTE: There's a lot going on in the result type. The (Either err b)
-- part allows syntax errors to be reported locally in the result as
-- `Nothing` operators. The (Either (esc, ...) ...) option allows us
-- to represent the result of parsing a closing parenthesis or an
-- unquote operation.
interpret ::
  (Functor m) => Env m esc err a b lit -> Expr m a lit ->
    Expr m (Either err b) (Either (esc, Expr m a lit) lit)
interpret env expr = case expr of
  Literal lit -> Literal (Right lit)
  Call op body -> callEnv env op body


coreEnv ::
  (SyntaxMaterial m) =>
  Env m esc String (Either CoreOp a) a lit ->
  (Either CoreOp a) ->
  Expr m (Either CoreOp a) (m (Expr m (Either CoreOp a) lit)) ->
    Expr m (Either String a)
      (Either (esc, Expr m (Either CoreOp a) lit) lit)
coreEnv env op body = case op of
  Right op' -> interpret env (Call (Right op') body)
  Left op' -> case op' of
    -- The idea of `coreInterpret` is that it's okay to make calls to
    -- whitespace-like operations like `OpId` itself inside the `OpId`
    -- region, but any other content doesn't make sense there.
    OpId -> case coreInterpret body of
      Call err body' -> resultIfIncorrectUsage err
      Literal lit -> case getOneAndOnly lit of
        Nothing -> resultIfIncorrectUsage
          "Called OpId with a number of arguments other than one"
        Just lit' -> interpret env lit'
    where
      resultIfIncorrectUsage err = Call (Left err) $
        exprMapLiteral (fmap $ interpret env) $ exprMapOp Left $
          coreInterpret body

exprMapLiteral ::
  (Functor m) => (a -> b) -> Expr m op a -> Expr m op b
exprMapLiteral func expr = case expr of
  Literal a -> Literal (func a)
  Call op body ->
    Call op $ exprMapLiteral (fmap (exprMapLiteral func)) body

exprMapOp :: (Functor m) => (a -> b) -> Expr m a lit -> Expr m b lit
exprMapOp func expr = case expr of
  Literal lit -> Literal lit
  Call op body ->
    Call (func op) $ exprMapLiteral (fmap (exprMapOp func)) $
      exprMapOp func body

coreInterpret :: (SyntaxMaterial m) =>
  Expr m (Either CoreOp a) lit -> Expr m String lit
coreInterpret expr = case expr of
  Literal lit -> Literal lit
  Call op body ->
    exprMapLiteral
      (\lit -> case lit of
        Left (esc, _) -> absurd esc
        Right lit' -> lit') $
    exprMapOp
      (\op -> case op of
        Left err -> err
        Right op' ->
          "Encountered a non-core operator where a core operator was expected") $
    callEnv (Env coreEnv) op body

-- This is a possible alternative to `coreInterpret` for cases where
-- even the core operators are not invited.
enforceLiteral :: (Functor m) => Expr m a lit -> Expr m String lit
enforceLiteral expr = case expr of
  Literal lit -> Literal lit
  Call op body ->
    Call "Encountered an operator where none was expected" $
      exprMapLiteral (fmap enforceLiteral) $ enforceLiteral body

data ExprAsFunctor lit m a =
  ExprAsFunctor {deExprAsFunctor :: (Expr m a lit)}

instance (Functor m) => Functor (ExprAsFunctor lit m) where
  fmap func (ExprAsFunctor expr) = ExprAsFunctor $ exprMapOp func expr

exprAsFunctorGetOneAndOnly ::
  (forall a. m a -> Maybe a) ->
    (forall a. ExprAsFunctor lit m a -> Maybe a)
exprAsFunctorGetOneAndOnly getOneAndOnly (ExprAsFunctor expr) =
  case expr of
    Literal lit -> Nothing
    Call op body -> case body of
      Call op' body' -> Nothing
      Literal body' -> getOneAndOnly body' >>= \body'' ->
        case body'' of
          Call op''' body''' -> Nothing
          Literal lit -> Just op

instance (SyntaxMaterial m) => SyntaxMaterial (ExprAsFunctor lit m)
  where
  getOneAndOnly = exprAsFunctorGetOneAndOnly getOneAndOnly

instance SyntaxMaterial Maybe where
  getOneAndOnly = id

listToExpr :: [a] -> ExprAsFunctor () Maybe a
listToExpr list = ExprAsFunctor $ case list of
  [] -> Literal ()
  x:xs -> Call x $ Literal $ Just $ deExprAsFunctor $ listToExpr xs

listFromExpr :: ExprAsFunctor () Maybe a -> Either String [a]
listFromExpr (ExprAsFunctor expr) = case expr of
  Literal () -> Right []
  Call x body -> case body of
    Call op' body' ->
      Left "Encountered content nested within a list element"
    Literal body' -> case body' of
      Nothing -> Left "Encountered no syntax following a list element"
      Just body'' ->
        (listFromExpr $ ExprAsFunctor body'') >>= \xs -> return (x:xs)

data SExpr a = SExprLiteral a | SExprList [SExpr a]
  deriving Show

sExprToExpr ::
  SExpr a -> ExprAsFunctor () (ExprAsFunctor () Maybe) (Maybe a)
sExprToExpr sExpr = ExprAsFunctor $ case sExpr of
  SExprLiteral a ->
    Call (Just a) $ Literal $ ExprAsFunctor $ Literal ()
  SExprList list ->
    Call Nothing $ Literal $ listToExpr $
      fmap (deExprAsFunctor . sExprToExpr) list

sExprFromExpr ::
  ExprAsFunctor () (ExprAsFunctor () Maybe) (Maybe a) ->
    Either String (SExpr a)
sExprFromExpr (ExprAsFunctor expr) = case expr of
  Literal () -> Left "Encountered no syntax"
  Call a body -> case a of
    Just a -> case body of
      Call op' body' ->
        Left "Encountered content nested within an s-expression literal's contour"
      Literal (ExprAsFunctor body') -> case body' of
        Call op' body' ->
          Left "Encountered content nested within an s-expression literal"
        Literal () -> Right (SExprLiteral a)
    Nothing -> case body of
      Call op' body' ->
        Left "Encountered content nested within an s-expression list's contour"
      Literal exprOfList -> do
        list <- listFromExpr exprOfList
        list' <- mapM (sExprFromExpr . ExprAsFunctor) list
        return $ SExprList list'

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

data CallList a = CallList a (Maybe (CallList a))
  deriving Show

instance Functor CallList where
  fmap func (CallList x xs) = CallList (func x) (fmap (fmap func) xs)

data CallSExpr a = CallSExpr a (Maybe (CallList (CallSExpr a)))
  deriving Show

instance Functor CallSExpr where
  fmap func (CallSExpr op body) =
    CallSExpr (func op) (fmap (fmap (fmap func)) body)

newtype ExprAsCallFunctor m a =
  ExprAsCallFunctor {
    deExprAsCallFunctor :: (Expr (Compose Maybe m) a Void)
  }

callListToExpr :: CallList a -> ExprAsCallFunctor Identity a
callListToExpr (CallList x xs) =
  ExprAsCallFunctor $ Call x $ Literal $ Compose $
    fmap (Identity . deExprAsCallFunctor . callListToExpr) xs

callListFromExpr ::
  ExprAsCallFunctor Identity a -> CallList (Either String a)
callListFromExpr (ExprAsCallFunctor expr) = case expr of
  Literal void -> absurd void
  Call x body -> case body of
    Call op' body' ->
      CallList
        (Left "Encountered content nested within a call-list element")
        Nothing
    Literal body' -> CallList (Right x) $
      fmap (callListFromExpr . ExprAsCallFunctor . runIdentity) $
      getCompose body'

callSExprToExpr ::
  CallSExpr a -> ExprAsCallFunctor (ExprAsCallFunctor Identity) a
callSExprToExpr (CallSExpr a list) =
  ExprAsCallFunctor $ Call a $ Literal $ Compose $
    fmap
      (callListToExpr . fmap (deExprAsCallFunctor . callSExprToExpr))
      list

callSExprFromExpr ::
  ExprAsCallFunctor (ExprAsCallFunctor Identity) a ->
    CallSExpr (Either String a)
callSExprFromExpr (ExprAsCallFunctor expr) = case expr of
  Literal void -> absurd void
  Call a body -> case body of
    Call op' body' ->
      CallSExpr
        (Left "Encountered content nested within a call-s-expression layer's contour")
        Nothing
    Literal body' ->
      CallSExpr (Right a) $ fmap (fmap recur . callListFromExpr) $
        getCompose body'
        where
          recur ::
            Either String
             (Expr (Compose Maybe (ExprAsCallFunctor Identity))
               a Void) ->
             CallSExpr (Either String a)
          recur errorExpr = case errorExpr of
            Left error -> CallSExpr (Left error) Nothing
            Right expr -> callSExprFromExpr $ ExprAsCallFunctor expr

showShownListToExpr :: [String] -> String
showShownListToExpr list =
  "(ExprAsFunctor " ++
    (case list of
      [] -> "(Literal ())"
      x:xs ->
        "(Call " ++ x ++ " $ Literal $ Just $ " ++
          "deExprAsFunctor " ++ showShownListToExpr xs ++ ")"
    ) ++ ")"

showListToExpr :: (Show a) => [a] -> String
showListToExpr = showShownListToExpr . fmap show

showSExprToExpr :: (Show a) => SExpr a -> String
showSExprToExpr sExpr =
  "(ExprAsFunctor " ++
    (case sExpr of
      SExprLiteral a ->
        "(Call (Just " ++ show a ++ ") $ Literal $ " ++
          "ExprAsFunctor $ Literal ())"
      SExprList list ->
        "(Call Nothing $ Literal " ++
          showShownListToExpr
            (fmap
              (\elem ->
                "(deExprAsFunctor " ++ showSExprToExpr elem ++ ")")
              list) ++ ")"
    ) ++ ")"

showShownCallListToExpr :: CallList String -> String
showShownCallListToExpr (CallList x xs) =
  "(ExprAsCallFunctor $ Call " ++ x ++ " $ Literal $ " ++
    "Compose " ++
    (case xs of
      Nothing -> "Nothing"
      Just xs' ->
        "$ Just $ Identity $ deExprAsCallFunctor " ++
          showShownCallListToExpr xs'
    ) ++ ")"

showCallListToExpr :: (Show a) => CallList a -> String
showCallListToExpr = showShownCallListToExpr . showList
  where
    showList (CallList x xs) = CallList (show x) (fmap showList xs)

showCallSExprToExpr :: (Show a) => CallSExpr a -> String
showCallSExprToExpr (CallSExpr a list) =
  "(ExprAsCallFunctor $ Call " ++ show a ++ " $ Literal $ " ++
    "Compose " ++
    (case list of
      Nothing -> "Nothing"
      Just list' ->
        "$ Just " ++
          showShownCallListToExpr
            (fmap
              (\expr ->
                "(deExprAsCallFunctor " ++
                  showCallSExprToExpr expr ++ ")")
              list')
    ) ++ ")"

-- listFromExpr $ listToExpr [1, 2, 3, 4]
-- showSExprToExpr $ SExprList [SExprList [SExprLiteral 1], SExprList [SExprLiteral 2]]
-- sExprFromExpr $ sExprToExpr $ SExprList [SExprList [SExprLiteral 1], SExprList [SExprLiteral 2]]
-- showCallListToExpr $ CallList 1 $ Just $ CallList 2 $ Just $ CallList 3 Nothing
-- callListFromExpr $ callListToExpr $ CallList 1 $ Just $ CallList 2 $ Just $ CallList 3 Nothing
-- showCallSExprToExpr $ CallSExpr 1 (Just $ CallList (CallSExpr 2 Nothing) $ Just $ CallList (CallSExpr 3 Nothing) Nothing)
-- callSExprFromExpr $ callSExprToExpr $ CallSExpr 1 (Just $ CallList (CallSExpr 2 Nothing) $ Just $ CallList (CallSExpr 3 Nothing) Nothing)
