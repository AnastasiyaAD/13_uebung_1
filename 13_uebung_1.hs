-----------------------------------------------------------------------------------------  3  -------------------------------------------------------------------------------
type Nat1 = Int -- Natural numbers starting from 1

type Numbers = Nat1 -- Values of dartboard segments

type Dartboard = [Numbers] -- Dartboard characterized by a list of purely ascending values

type Turn = [Numbers] -- Reached scores of a turn (Wurffolge); only
                      -- scores occurring on the dartboard are possible,
                      -- also more than once.

type Turns = [Turn] -- Stream of turns

type TargetScore = Nat1 -- Desired overall score > 0

type Throws = Nat1 -- Number of darts of a turn > 0

gen_turns :: Dartboard -> Turns -- Generates a stream of turns in accordance with the numbered
                                -- segments of the dartboard, e.g., all turns with 1 dart, subsequently all
                                -- turns with 2 darts, etc. Take care that your generator is fair and does not
                                -- generate duplicates, i.e., generates every turn with a finite number of darts
                                -- eventually, while not generating duplicates of turns in form of permutations
                                -- (turns like [7,23,12], [23,7,12], [12,7,23], etc. are considered duplicates; only
                                -- one of them shall be generated).
gen_turns [] = [[]] 
gen_turns dartboard = do 
    n <- [1..5]  -- limitation for testing functions
    gen_turns' n dartboard

gen_turns' :: Int -> [Numbers] -> [[Numbers]] 
gen_turns' 0 _ = [[]]
gen_turns' 1 xs = [ x : comb | x <- xs, comb <- gen_turns' 0 xs ]-- n:  the number of elements in the combination
                                                                      -- xs: the list from which the elements for combinations are selected
                                                                      -- x : comb -- it takes the element x and adds it to the beginning of the combination comb
gen_turns' n xs = [ x : comb | x <- xs, comb <- gen_turns' (n - 1) xs, x >= head comb ] 

filter_turns_ts :: Turns -> TargetScore -> Turns -- Filters the input stream for those turns, whose summed overall score matches the target score.
filter_turns_ts [[]] score = [[]]
filter_turns_ts turns score =  filter (\turn -> sum turn == score) turns

filter_turns_th :: Turns -> Throws -> Turns -- Filters the input stream for those turns with the given number of darts.
filter_turns_th [[]] n = [[]]
filter_turns_th turns n =  filter (\turn -> length turn == n) turns

select_turns_minl :: Turns -> Turns -- Picks from the input list the turns with the smallest number of darts.
select_turns_minl [[]] = [[]]
select_turns_minl turns = 
    let minLength = minimum (map length turns)
    in filter (\turn -> length turn == minLength) turns

--(Two further Notes on Assignment 1) Any ordering in which your functions deliver their results is fine.
-- transf_sort_turns :: Turns -> Turns -- Sorts the turns of the input stream descendingly.

---------------------------------------------------------------------------------------  3.1  -------------------------------------------------------------------------------
dart_ts :: Dartboard -> TargetScore -> Turns --  yields the (finite number of) turns reaching the target score.
dart_ts board targetScore = filter_turns_ts (gen_turns board) targetScore
---------------------------------------------------------------------------------------  3.2  -------------------------------------------------------------------------------
dart_tst :: Dartboard -> TargetScore -> Throws -> Turns -- yields the (finite number of) turns reaching the target score with the given number of darts.
dart_tst board targetScore throws = filter_turns_th (dart_ts board targetScore) throws
---------------------------------------------------------------------------------------  3.3  -------------------------------------------------------------------------------
dart_tsml :: Dartboard -> TargetScore -> Turns -- yields the (finite number of) turns reaching the target score with the smallest number of darts.
dart_tsml board targetScore = select_turns_minl (dart_ts board targetScore)



main :: IO ()
main = do
    let db, db' :: Dartboard
        db = [6,7,16,17,26,27,36,37,46,47]
        db' = [1,2,3]
        turns = gen_turns db
    putStrLn "-------------  3  -------------"
    putStrLn ""
    putStrLn $ show (gen_turns' 2 db')
    putStrLn $ show (filter_turns_ts turns 23)  
    putStrLn $ show (filter_turns_th turns 1)
    putStrLn $ show (select_turns_minl turns)
    putStrLn $ show (dart_ts db 23)     
    putStrLn $ show (dart_tst db 55 4)    
    putStrLn $ show (dart_tsml db 100) 
    putStrLn $ show (dart_ts db 15)
    putStrLn $ show (dart_ts db 27)