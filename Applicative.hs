import Text.Read

-- demo fmap over Maybe Double
demo1 = do
        l <- getLine
        let maybeDouble = fmap (*2) (readMaybe l :: Maybe Double)
        case maybeDouble of
            Just d -> putStrLn (show d)
            otherwise -> putStrLn "nothing"


-- demo applicative: apply function to Maybe and then apply another Maybe
demo2 = do
        l1 <- getLine
        l2 <- getLine
        let d1 = readMaybe l1 :: Maybe Double
            d2 = readMaybe l2
        let maybeSum = fmap (+) d1 <*> d2
        case maybeSum of
            Just s -> putStrLn (show s)
            otherwise -> putStrLn "nothing"

demo2' = do
         d1 <- fmap readMaybe getLine
         d2 <- fmap readMaybe getLine
         let maybeSum = (+) <$> d1 <*> d2 :: Maybe Double
         case maybeSum of
             Just d -> putStrLn (show d)
             otherwise -> putStrLn "nothing"


-- demo appliative: apply concatination function to IO String and then apply another IO String
demo3 = do
        s1 <- getLine
        s2 <- getLine
        putStrLn (s1 ++ s2)

demo3' = do
         s <- (++) <$> getLine <*> getLine
         putStrLn s


-- demo applicative: sequencing operator *>
demo4 = (\_ y -> y) <$> putStrLn "First" <*> putStrLn "Second"

demo4' = putStrLn "First" *> putStrLn "Second"


-- demo applicative: sequencing operator *> in withing do notation
demo5 = do
        putStrLn "choose 2 strings"
        s <- (++) <$> getLine <*> getLine
        putStrLn s

demo5' = do
         s <- putStrLn "choose 2 strings" *> ((++) <$> getLine <*> getLine)
         putStrLn s

