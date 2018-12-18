import Control.Monad.State

-- credits: https://github.com/BinaryTENSHi

data Record = Entry String Integer
              deriving(Show)

type Book = [Record]

find' :: String -> [Record] -> Maybe Integer
find' name [] = Nothing
find' name ((Entry ename enumber) : rest) = if name == ename
                                            then Just enumber
                                            else find' name rest

find :: String -> State Book (Maybe Integer)
find name = state $ \book -> (find' name book, book)

add :: Record -> State Book ()
add record = state $ \book -> ((), record:book)

-- do notation
demo :: State Book (Maybe Integer)
demo = do
    add $ Entry "One" 1234
    add $ Entry "Two" 5678
    find "One"

-- then operator
demo2 :: State Book (Maybe Integer)
demo2 = 
    add (Entry "One" 1234) >>
    add (Entry "Two" 5678) >>
    find "One"

-- to try it out: 
-- evalState demo []
-- execState demo []
