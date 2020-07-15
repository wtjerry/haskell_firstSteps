module MostImperativeLanguageEver
  ( def,
    var,
    lit,
    while,
    (+=),
    (-=),
    (*=),
  )
where

import Control.Monad.State
import qualified Data.Map.Strict as M

newtype Index = Index Int deriving (Eq, Ord)

type Env = M.Map Index Integer

type ImperativeMonad = State Env

data ImperativeVariable = MkVar {getIdx :: Index}

data ImperativeLiteral = MkLit Integer

class Var a where
  getValue :: a -> Env -> Integer

instance Var ImperativeVariable where
  getValue (MkVar i) env = env M.! i

instance Var ImperativeLiteral where
  getValue (MkLit v) _ = v

def :: Var v => ImperativeMonad v -> Integer
def x =
  let (v, e) = runState x M.empty
   in getValue v e

var :: Integer -> ImperativeMonad ImperativeVariable
var val = do
  env <- get
  let idx = Index $ M.size env
  let newEnv = M.insert idx val env
  put newEnv
  let impVar = MkVar idx
  return impVar

while :: ImperativeVariable -> (Integer -> Bool) -> ImperativeMonad () -> ImperativeMonad ()
while v predicate action = do
  env <- get
  let val = env M.! (getIdx v)
  when (predicate val) $ do
    let newEnv = execState action env
    put newEnv
    while v predicate action

lit :: Integer -> ImperativeLiteral
lit = MkLit

(+=) :: Var v => ImperativeVariable -> v -> ImperativeMonad ()
(+=) = modifyingOperation (+)

(-=) :: Var v => ImperativeVariable -> v -> ImperativeMonad ()
(-=) = modifyingOperation (-)

(*=) :: Var v => ImperativeVariable -> v -> ImperativeMonad ()
(*=) = modifyingOperation (*)

modifyingOperation :: Var v => (Integer -> Integer -> Integer) -> ImperativeVariable -> v -> ImperativeMonad ()
modifyingOperation op a b =
  do
    env <- get
    let aVal = getValue a env
    let bVal = getValue b env
    let newAVal = aVal `op` bVal
    let aIdx = getIdx a
    let newState = M.adjust (const newAVal) aIdx env
    put newState
    return ()
