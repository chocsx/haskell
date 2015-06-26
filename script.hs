module Script where


import Data.List

memory =  replicate 256 "0000"  -- 256 positions on 'memory'

ac2mem :: Int -> String -- position number -> string of the list
ac2mem n
        |n == 0 = memory !! 0 -- if n=0 access position zero of the list
        |n < 256 = memory !! n -- if n>0 access position n of the list
        
splitList :: Int -> [a] -> ([a], [a]) -- position of the list -> list ->(list1, list2)
splitList 0 xs     = ([], xs) -- if split first item of the list add [] on the respective position
splitList _ []     = ([], [])-- position and list empty   
splitList n (x:xs) = (x:xs', xs'') -- position n (head:tail) = (head:tail', tail'')
  where
    (xs', xs'') = splitAt (n - 1) xs -- split on position before the 'n'
    
replaceAtIndex :: Int -> a -> [a] -> [a] -- position, String, list, newList
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls
   


    
--d2m :: String -> [a] - >[a]
--d2m x xs = 