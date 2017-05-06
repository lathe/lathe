-- macros.hs

-- This file isn't meant to become a Haskell library (yet, anyway).
-- It's just a scratch area to help guide the design of macros.rkt.


{-# LANGUAGE RankNTypes #-}

-- {-# LANGUAGE ExistentialQuantification #-}

import Data.Void

{-

class (Monad atree, Monad btree) =>
  Interpretable env esc err op atree aleaf btree bleaf where
  
  toMacroCall ::
    atree aleaf' -> Either aleaf' (op, atree (atree aleaf'))
  interpretLeaf :: aleaf -> bleaf
  idOp ::
    env -> atree (atree aleaf) ->
      btree (Either bleaf (esc, atree aleaf))
  unwrapEnv ::
    env -> env -> op -> atree (atree aleaf) ->
      btree (Either bleaf (esc, atree aleaf))
  wrapEnv ::
    (env -> op -> atree (atree aleaf) ->
      btree (Either bleaf (esc, atree aleaf))
    ) ->
      env
  
  interpret ::
    env -> atree aleaf ->
      btree (Either bleaf (esc, atree aleaf))
  interpret env expr = case toMacroCall expr of
    Left aleaf -> return (Left (interpretLeaf aleaf))
    Right (op, body) -> unwrapEnv env env op body
  
  unwrapEnv (wrapEnv unwrappedEnv) ~ unwrappedEnv
  wrapEnv (unwrapEnv wrappedEnv) ~ wrappedEnv
  idOp env body ~ case toMacroCall body of
    Left following -> case toMacroCall following of
      Left leaf -> return (Left (interpretLeaf leaf))
      Right (op, following')
        return (Right (esc, following'))
    Right _ -> TODO

-}


{-

data Expr m a lit = Literal lit | Call a (m (Expr m a lit))

exprLiteralDone :: (Monad m) => lit -> Expr m a lit
exprLiteralDone result = Literal result
exprLiteralBind :: (Monad m) => Expr m a lita -> (lita -> Expr m a litb) -> Expr m a litb
exprLiteralBind effects next = case effects of
  Literal lit -> next lit
  Call op body -> Call op (fmap (\body' -> exprLiteralBind body' next) body)
instance (Monad m) => Monad (Expr m a) where
  return = exprLiteralDone
  (>>=) = exprLiteralBind

exprOpDone :: (Monad m) => a -> Expr m a ()
exprOpDone result = Call result (return (Literal ()))
exprOpBind :: (Monad m) => Expr m a () -> (a -> Expr m b ()) -> Expr m b ()
exprOpBind effects next = case effects of
  Literal () -> Literal ()
  Call op body -> 
  Call op body -> Call op (body >>= \body' -> exprLiteralBind body' next)




data Expr m a lit = Literal lit | Call a (Expr m a (m (Expr m a lit)))
data MExpr m a lit = MExpr (m (Expr m a lit))

deMExpr :: MExpr m a lit -> m (Expr m a lit)
deMExpr (MExpr internal) = internal

exprLiteralDone :: (Monad m) => lit -> MExpr m a lit
exprLiteralDone x = MExpr (return (Literal x))
exprLiteralBind :: (Monad m) => MExpr m a lita -> (lita -> MExpr m a litb) -> MExpr m a litb
exprLiteralBind (MExpr effects) next = MExpr $ effects >>= \effects' -> deMExpr $ case effects' of
  Literal lit -> next lit
  Call op body ->
    exprLiteralBind (MExpr (return body)) $ \leaf ->
    exprLiteralBind (MExpr leaf) next
instance (Monad m) => Monad (MExpr m a) where
  return = exprLiteralDone
  (>>=) = exprLiteralBind

-}

{-
exprOpDone :: (Monad m) => a -> (forall lit. lit -> Expr m a lit)
exprOpDone x = \lit -> Call op (Literal (return (Literal lit)))
exprOpBind ::
  (Monad m) =>
  (forall lit. lit -> Expr m a lit) ->
  (a -> (forall lit. lit -> Expr m b lit)) ->
    (forall lit. lit -> Expr m b lit)
exprOpBind effects next = \lit -> case effects lit of
  Literal lit' => Literal lit'
  Call op body =>
    -- continuation is type (Expr m b lit)
    -- op is type a
    -- next is type (a -> (forall lit. lit -> Expr m b lit))
    -- body is type (Expr m a (Expr m a lit))
exprOpBind :: (Monad m) => Expr m a Unit -> (a -> Expr m b Unit) -> Expr m b Unit
exprOpBind effects next = case effects of
  Literal lit => Literal lit
  Call op body =>
    -- continuation is type (Expr m b lit)
    -- op is type a
    -- next is type (a -> (forall lit. Expr m b lit))
    -- body is type (Expr m a (Expr m a lit))
-}

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

coreInterpret ::
  (SyntaxMaterial m) =>
  Expr m (Either CoreOp a) lit ->
    Expr m String lit
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

data ExprAsFunctor m lit a = ExprAsFunctor (Expr m a lit)
instance (Functor m) => Functor (ExprAsFunctor m lit) where
  fmap func (ExprAsFunctor expr) = ExprAsFunctor $ exprMapOp func expr

exprAsFunctorGetOneAndOnly ::
  (forall a. m a -> Maybe a) ->
    (forall a. ExprAsFunctor m lit a -> Maybe a)
exprAsFunctorGetOneAndOnly getOneAndOnly (ExprAsFunctor expr) =
  case expr of
    Literal lit -> Nothing
    Call op body -> case body of
      Call op' body' -> Nothing
      Literal body' -> getOneAndOnly body' >>= \body'' ->
        case body'' of
          Call op''' body''' -> Nothing
          Literal lit -> Just op

instance (SyntaxMaterial m) => SyntaxMaterial (ExprAsFunctor m lit) where
  getOneAndOnly = exprAsFunctorGetOneAndOnly getOneAndOnly
