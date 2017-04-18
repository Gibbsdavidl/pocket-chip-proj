import System.Environment
import Data.List
import Data.Sequence as Seq

-- problem 9

shead :: Seq Integer -> Integer
shead x
    | Seq.length x == 0 = 0
    | otherwise = (Seq.index x 0)

-- roll with head of prime list
roll' :: Integer -> Seq Integer -> Seq Integer -> Seq Integer
roll' x nums newNums
    | Seq.length nums <= 0 = newNums
    | mod (shead nums) x == 0 = roll' x (Seq.drop 1 nums) newNums -- it divides, drop it
    | otherwise  = roll' x (Seq.drop 1 nums) (newNums Seq.|> a') -- keep rolling
    where a' = shead nums

-- roll the new wheel
roll :: Integer ->  Seq Integer -> Seq Integer
roll x nums = roll' x nums (Seq.empty)

-- update the annotations ... 1 means its a factor
-- if as are all 0's then just return ps
updateList :: [Seq Integer] -> [Seq Integer]
updateList [ps,as] = [ (newps Seq.<| ps) , as' ]
    where as'   = roll (shead ps) as
          newps = shead as'

-- until head of prime list is maxn
-- keep updating our list
makePlist :: [Seq Integer] -> Seq Integer
makePlist plist
    | Seq.length (last plist) == 0 = Seq.dropWhileL (==0) (head plist) -- done
    | otherwise = makePlist (updateList plist)  -- keep filtering

-- roll the wheel (the last prime we found) on the index vector
-- get the new prime ... it's the first '1' + the offset
-- set the new offset, it's the last prime -1
-- reduce the index vector upto the offset.
   -- then the vector we roll on gets smaller and smaller.

main = do
  args <- getArgs
  let maxn = read (head args) :: Integer
  let prime_list =  Seq.singleton 2
  let index_list = Seq.fromList [2 .. maxn]
  let plist = [prime_list, index_list]
  print $ makePlist plist
