module Libs.Asslist
    (AssList
    ,lookup
    ,adjust
    ,update
    ,delete
    ,member
    ,clean
    ,values
    ,keys
    ) where

type AssList a b = [(a,b)]

keys :: AssList k v -> [k]
keys = map fst

values :: AssList k v -> [v]
values = map snd

lookup :: Eq k => k -> AssList k v -> Maybe v
lookup _ [] = Nothing
lookup k ((k1,v):ks) = if k == k1 then v else (lookup k ks)

adjust :: Eq k => (v -> v) -> k -> AssList k v -> AssList k v
adjust _ _ [] = []
adjust f k ((k1,v):ks) = if k == k1 then (k,f v):ks else (k1,v):ks

update :: Eq k => (v -> Maybe v) -> k -> AssList k v -> AssList k v
update _ _ [] = []
update f k ((k1,v):ks) = if k == k1
                            then case (f v) of
                                   Just x -> (k,x):ks
                                   Nothing -> ks
                            else (k1,v):(update f k ks)

delete :: Eq k => k -> AssList k v -> AssList k v
delete = update (\_ -> Nothing)

member :: Eq k => AssList k v -> k -> Bool
member al k = k `elem` (keys al)

-- (v -> v -> v) should be in the form of f x1 x2 where x2 is later in the list
clean :: Eq k => (v -> v -> v) -> AssList k v -> AssList k v
clean _ [] = []
clean f ((k1,v):ks) = if k1 `member` ks
                         then do { let kmatches = map snd $ filter (\(k,_) -> k == k1) ks
                                 ; let v' = foldl f v kmatches
                                 ; (k1,v'):(clean f ks)
                                 }
                         else (k1,v):(clean f ks)
