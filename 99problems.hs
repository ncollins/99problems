
-- Problem 1.
-- A function that returns the last element of a list

myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- Problem 2.
-- Find the last but one element of a list.

myButLast (x:y:[]) = x
myButLast (x:xs)   = myButLast xs

-- Problem 3.
-- Find the kth element of a list, indexed from 1.

elementAt (x:xs) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)

-- Problem 4.
-- length of a list.

myLength ys =
    let recur acc xs =
            case (acc, xs) of (n, []) -> acc
                              (n, _:xs') -> recur (n + 1) xs'
    in
       recur 0 ys

myLength' xs =
    recur 0 xs
        where recur acc [] = acc
              recur acc (_:xs') = recur (1 + acc) xs'

-- Problem 5.
-- reverse a list

myReverse [] = []
myReverse xs = recur [] xs
    where recur acc [] = acc
          recur acc (x:xs') = recur (x:acc) xs'
    
myReverse' xs = foldl (\acc x -> x:acc) [] xs

-- Problem 6.
-- check if a list is a palindrome.

isPalindrome xs = xs == (reverse xs)

-- Problem 7.
-- flatten a list

data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = (myFlatten x) ++ (myFlatten (List xs))

-- Problem 8.
-- remove consecutive duplicated

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:y:rest) = if x == y
                      then compress (y:rest)
                      else x:(compress (y:rest))

-- Problem 9.
-- pack consecitive duplicates into sublists

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = reverse (packacc [] [] xs)
    where packacc list elems [] = elems:list
          packacc list [] (x:xs') = packacc list [x] xs'
          packacc list (e:es) (x:xs') = if e == x
                                        then packacc list (e:e:es) xs'
                                        else packacc ((e:es):list) [x] xs'

-- Problem 10.
-- run length encoding.

encode xs = map (\ys -> (length ys, head ys)) (pack xs)

-- Problem 11.
-- modified length encoding.

data Variable a = Multiple Int a | Single a deriving (Show)

encodeModified xs = map convert (pack xs)
    where convert [y] = Single y
          convert ys = Multiple (length ys) (head ys)

-- Problem 12.
-- decode modified length.

decodeModified vs = foldl (\acc v -> acc ++ (conv v)) [] vs
    where conv (Single v) = [v]
          conv (Multiple n v) = take n $ repeat v

-- Problem 13.
-- encode directly.

encodeDirect xs = reverse $ encodeAcc [] xs
    where encodeAcc acc [] = acc
          encodeAcc [] (x:xs) = encodeAcc [Single x] xs
          encodeAcc ((Single v):vs) (x:xs) = if x == v
                                             then encodeAcc ((Multiple 2 v):vs) xs
                                             else encodeAcc ((Single x):(Single v):vs) xs
          encodeAcc ((Multiple n v):vs) (x:xs) = if x == v
                                                 then encodeAcc ((Multiple (n + 1) v):vs) xs
                                                 else encodeAcc ((Single x):(Multiple n v):vs) xs

-- Problem 14.
-- duplicate the elements of a list.

dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- Problem 15.
-- replicate the elements of a list.

repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ (repli xs n)

-- Problem 16.
-- drop every Nth element of a list.

myDrop [] _ = []
myDrop xs n = recur xs 1
    where recur [] _ = [] 
          recur (x:xs) count = if count == n
                               then recur xs 1
                               else x:(recur xs (count + 1))

-- Problem 17.
-- split a list.

mySplit [] _ = ([], [])
mySplit (x:xs) 1 = ([x], xs)
mySplit (x:xs) n = let (a, b) = mySplit xs (n - 1)
                   in (x:a, b)

-- Problem 18.
-- get a slice from a list.

slice [] l u = []
slice (x:xs) l u
    | u < 1     = []
    | l <= 1    = x:(slice xs (l-1) (u-1))
    | otherwise = slice xs (l-1) (u-1)

-- Problem 19.
-- rotate a list N places

rotate [] _ = []
rotate xs n = let shift = n `mod` (length xs)
                  (l, r) = mySplit xs shift
              in r ++ l

-- Problem 20.
-- remove the Nth element of a list
-- (I decided to use Maybe rather than errors in the case of no Nth element)

removeAt _ [] = (Nothing, [])
removeAt n (x:xs)
    | n < 1     = (Nothing, x:xs)
    | n == 1    = (Just x, xs)
    | otherwise = let (e, ys) = removeAt (n-1) xs
                  in (e, x:ys)

-- Problem 21.
-- insert element at given position into list

insertAt e [] n = if n == 1
                  then [e]
                  else error "insertAt: invalid index"
insertAt e (x:xs) n
  | n == 1 = e:x:xs
  | n > 1  = x:(insertAt e xs (n-1))
  | n < 1  = error "insertAt: invalid index"

-- Problem 22.
-- a range function for integers

range low high
  | low > high = []
  | otherwise  = low:(range (low+1) high)
