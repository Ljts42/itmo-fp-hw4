module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import qualified Control.Monad
import           HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState func (ES run) = ES $ mapExcept (mapAnnotated func) . run

wrapExceptState :: a -> ExceptState e s a
wrapExceptState value = ES $ \s -> Success (value :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES run) = ES $ \s -> case run s of
  (Error er)                   -> Error er
  (Success (es :# annotation)) -> runES es annotation

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState func = ES $ \s -> Success (() :# func s)

throwExceptState :: e -> ExceptState e s a
throwExceptState er = ES $ \_ -> Error er

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  es >>= func = joinExceptState $ fmap func es

data EvaluationError = DivideByZero
  deriving Show

getES :: Prim Double -> Double -> ExceptState EvaluationError [Prim Double] Double
getES (Div _ 0) _ = throwExceptState DivideByZero
getES expr res = do
  modifyExceptState (expr :)
  pure res

binary :: (Double -> Double -> Prim Double) -> Expr -> Expr -> (Double -> Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
binary prim a b op = do
  x <- eval a
  y <- eval b
  getES (prim x y) (op x y)

unary :: (Double -> Prim Double) -> Expr -> (Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
unary prim a op = do
  x <- eval a
  getES (prim x) (op x)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val num)      = pure num
eval (Op (Add a b)) = binary Add a b (+)
eval (Op (Sub a b)) = binary Sub a b (-)
eval (Op (Mul a b)) = binary Mul a b (*)
eval (Op (Div a b)) = binary Div a b (/)
eval (Op (Abs a))   = unary Abs a abs
eval (Op (Sgn a))   = unary Sgn a signum
