{-
    Draghici Vlad Matei 322CB

    PP Project 2021

    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Dataset
import Text.Printf
import Data.List
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
    TASK SET 1
-}
evalStringToFloat :: String -> Float
evalStringToFloat "" = 0
evalStringToFloat x = (read x :: Float)

evalSort :: String -> Float
evalSort "" = -1
evalSort x = (read x :: Float)


-- Task 1.1
compute_exam_grades :: Table -> Table
compute_exam_grades = \x -> ["Nume", "Punctaj Exam"] : (map opRow (tail x))
    where
        opRow :: Row -> Row
        opRow = \x -> (head x) : (printf "%.2f" (compute_grade (tail x))) : []
            where
                compute_grade :: Row -> Float
                compute_grade = \x -> (sum (map evalStringToFloat (init x)) / 4)
                 + (evalStringToFloat (last x))
                        
-- Task 1.2
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num = \x -> length (filter (\x -> (read (head (tail x)) :: Float)
 > 2.5) (tail (compute_exam_grades x)))

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage = \x -> (fromIntegral.get_passed_students_num) x /
 (fromIntegral (length x - 1))

noColumnsTable :: Table -> Int
noColumnsTable = (length.head)

getColumnsInterval :: Table -> Int -> Int -> Table
getColumnsInterval t x y = elimNEndColumns ((noColumnsTable t) - y) (elimNFrontColumns (x - 1) t)
    where
        elimNFrontColumns :: Int -> Table -> Table
        elimNFrontColumns 0 t = t
        elimNFrontColumns x t = map (\x -> tail x) (elimNFrontColumns (x - 1) t)

        elimNEndColumns :: Int -> Table -> Table
        elimNEndColumns 0 t = t
        elimNEndColumns x t = map (\x -> init x) (elimNEndColumns (x - 1) t)

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg = \x -> sum (map (\x -> read (last x) :: Float) ((tail.compute_exam_grades) x))
 / (fromIntegral (length x - 1))

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num = \x -> length (filter (\x -> x >= 1.5) (map (\x -> sum (map
 evalStringToFloat x)) (tail $ getColumnsInterval x 3 5)))

getFloatValueColumn :: Table -> Int -> [Float]
getFloatValueColumn t x = map (\x -> (evalStringToFloat.head) x) (getColumnsInterval t x x)

getStringValueColumn :: Table -> Int -> Row
getStringValueColumn t x = map (\x -> (head) x) (getColumnsInterval t x x)

getAvgColumn :: Table -> Int -> String
getAvgColumn t x = printf "%.2f" $ (sum $ getFloatValueColumn t x) / (fromIntegral $ length t)

-- Task 1.3
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs = \x -> ["Q1","Q2","Q3","Q4","Q5","Q6"]:((getAvgColumn
 (tail x) 2):(getAvgColumn (tail x) 3):(getAvgColumn (tail x) 4):(getAvgColumn (tail x) 5)
 :(getAvgColumn (tail x) 6):(getAvgColumn (tail x) 7):[]):[]

getPointsQ :: Table -> Int -> Row
getPointsQ t x = (show $ length $ filter (\x -> x == 0) (getFloatValueColumn t x)) : (show $
 length $ filter (\x -> x == 1) (getFloatValueColumn t x)) : (show $ length $ filter (\x ->
 x == 2) (getFloatValueColumn t x)) : []

-- Task 1.4
get_exam_summary :: Table -> Table
get_exam_summary t = ["Q","0","1","2"] : ("Q1" : (getPointsQ (tail t) 2)) : ("Q2" : (getPointsQ
 (tail t) 3)) : ("Q3" : (getPointsQ (tail t) 4)) : ("Q4" : (getPointsQ (tail t) 5)) : ("Q5" :
 (getPointsQ (tail t) 6)) : ("Q6" : (getPointsQ (tail t) 7)) : []

cmpLastIncRow :: Row -> Row -> Ordering
cmpLastIncRow x y
    | (evalStringToFloat (last x)) < (evalStringToFloat (last y)) || ((evalStringToFloat
     (last x)) == (evalStringToFloat (last y)) && (head x) < (head y)) = LT
    | otherwise = GT

-- Task 1.5
get_ranking :: Table -> Table
get_ranking t = (head (compute_exam_grades t)) : (sortBy cmpLastIncRow (tail (compute_exam_grades t)))

-- Task 1.6
get_exam_diff_table :: Table -> Table
get_exam_diff_table = \x -> ["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"]
 : (sortBy cmpLastIncRow (map opRow (tail x)))
    where
        opRow :: Row -> Row
        opRow = \x -> (head x) : (printf "%.2f" (compute_interviu (tail x))) : (printf "%.2f"
         (compute_scris (tail x))) : ((printf "%.2f".abs) ((compute_interviu (tail x)) -
         (compute_scris (tail x)))) :[]
            where
                compute_interviu :: Row -> Float
                compute_interviu = \x -> (sum (map evalStringToFloat (init x)) / 4)

                compute_scris :: Row -> Float
                compute_scris = \x -> evalStringToFloat (last x)

{-
    TASK SET 2
-}

splitBy :: Char -> String -> [String]
splitBy myChar x = foldr splitOp [] x
    where
        splitOp x []
            | (x == myChar) = []:[]:[]
            | otherwise = [[x]]
        splitOp x (y:ys)
            | (x == myChar) = []:y:ys
            | otherwise = (x:y):ys
        
-- Task 2.0
read_csv :: CSV -> Table
read_csv csv = map (splitBy ',') (splitBy '\n' csv)

write_csv :: Table -> CSV
write_csv table = init $ foldr (\x y-> (f x) ++ ['\n'] ++ y) "" table
    where
        f row = init $ foldr (\x y -> x ++ [','] ++ y) "" row

-- Task 2.1
as_list :: String -> Table -> [String]
as_list str table = tail $ head $ filter (\x -> (head x) == str) $ transpose table

-- Task 2.2

get_kth_elem :: Int -> Row -> Value
get_kth_elem _ [] = ""
get_kth_elem 1 (x:xs) = x
get_kth_elem k (x:xs) = get_kth_elem (k - 1) xs

get_column_index :: String -> Row -> Int
get_column_index str (x:xs)
    | (str == x) = 1
    | otherwise = 1 + get_column_index str xs

tsort :: String -> Table -> Table
tsort str table = (head table) : (sortBy cmpRows (tail table))
    where
        cmpRows :: Row -> Row -> Ordering
        cmpRows r1 r2
            | evalSort (get_kth_elem (get_column_index str (head table)) r1)
             < evalSort (get_kth_elem (get_column_index str (head table)) r2) = LT
            | evalSort (get_kth_elem (get_column_index str (head table)) r1)
             == evalSort (get_kth_elem (get_column_index str (head table)) r2)
             && ((head r1) <= (head r2)) = LT
            | otherwise = GT

-- Task 2.3
vmap :: (Value -> Value) -> Table -> Table
vmap f table = map (\x -> map f x) table

-- Task 2.4
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f header table = header : (map f (tail table))

get_hw_grade_total :: Row -> Row
get_hw_grade_total row = (head row) : (printf "%.2f" $ sum (map evalStringToFloat
 (tail $ tail row))) : []

-- Task 2.5
vunion :: Table -> Table -> Table
vunion t1 t2 = if ((head t1) == (head t2)) then t1 ++ (tail t2) else t1

-- Task 2.6
hunion :: Table -> Table -> Table
hunion t1 t2 = if((length t1) < (length t2)) then (zipWith (++) (extend t1 ((length t2)
 - (length t1))) t2) else (zipWith (++) t1 (extend t2 ((length t1) - (length t2))))

extend :: Table -> Int -> Table
extend table x = table ++ (take x $ repeat (take (length $ head table) $ repeat []))

elimAtIndex :: Int -> [a] -> [a]
elimAtIndex x l = (take (x - 1) l) ++ (drop x l)

-- Task 2.7
tjoin :: String -> Table -> Table -> Table
tjoin str t1 t2 = map f t1
    where
        f :: Row -> Row
        f my_row = if ((length $ filter same_column t2) > 0) then (my_row ++ (elimAtIndex
         (get_column_index str (head t2)) (head $ filter same_column t2))) else (my_row ++
         (take ((length $ head t2) - 1) $ repeat []))
            where
                same_column :: Row -> Bool
                same_column other_row = (get_kth_elem (get_column_index str (head t1)) my_row)
                 == (get_kth_elem (get_column_index str (head t2)) other_row)

-- Task 2.8
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f header t1 t2 = header : (foldr (++) [] (map (\x -> map (\y -> f x y)
 (tail t2)) (tail t1)))

-- Task 2.9
projection :: [String] -> Table -> Table
projection strs table = transpose $ map (\x -> head $ filter (\y -> x == (head y))
 (transpose table)) strs

{-
    TASK SET 3
-}

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query
 
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]

-- 3.1
instance Show QResult where
    show (CSV csv) = show csv
    show (Table table) = write_csv table
    show (List list) = show list

class Eval a where
    eval :: a -> QResult

-- 3.2
instance Eval Query where
    eval (FromCSV csv) = Table $ read_csv $ csv

    eval (ToCSV query) = CSV $ show $ eval $ query

    eval (AsList colname query) = List $ as_list colname (read_csv $ show $ eval $ query)

    eval (Sort colname query) = Table $ tsort colname (read_csv $ show $ eval $ query)

    eval (ValueMap op query) = Table $ vmap op (read_csv $ show $ eval $ query)

    eval (RowMap op colnames query) = Table $ rmap op colnames (read_csv $ show $ eval $ query)
    
    eval (VUnion query1 query2) = Table $ vunion (read_csv $ show $ eval $ query1)
     (read_csv $ show $ eval $ query2)

    eval (HUnion query1 query2) = Table $ hunion (read_csv $ show $ eval $ query1)
     (read_csv $ show $ eval $ query2)
    
    eval (TableJoin colname query1 query2) = Table $ tjoin colname (read_csv $
     show $ eval $ query1) (read_csv $ show $ eval $ query2)

    eval (Cartesian op colnames query1 query2) = Table $ cartesian op colnames
     (read_csv $ show $ eval $ query1) (read_csv $ show $ eval $ query2)

    eval (Projection colnames query) = Table $ projection colnames (read_csv $
     show $ eval $ query)

    eval (Filter fCond query) = Table $ (head (read_csv $ show $ eval $ query))
     : (filter (feval (head (read_csv $ show $ eval $ query)) fCond) (tail (read_csv
      $ show $ eval $ query)))

    eval (Graph edgeop query) = Table $ create_graph edgeop (read_csv $ show $ eval $ query)

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
    feval header (Eq colname ref) = \row -> ((evalStringToFloat (get_kth_elem
     (get_column_index colname header) row)) == ref)

    feval header (Lt colname ref) = \row -> ((evalStringToFloat (get_kth_elem
     (get_column_index colname header) row)) < ref)

    feval header (Gt colname ref) = \row -> ((evalStringToFloat (get_kth_elem
     (get_column_index colname header) row)) > ref)

    feval header (In colname list) = \row -> (elem (evalStringToFloat (get_kth_elem
     (get_column_index colname header) row)) list)

    feval header (FNot cond) = \row -> (not $ (feval header cond) row)

    feval header (FieldEq colname1 colname2) = \row -> ((get_kth_elem (get_column_index
     colname1 header) row) == (get_kth_elem (get_column_index colname2 header) row))

instance FEval String where
    feval header (Eq colname ref) = \row -> ((get_kth_elem (get_column_index
     colname header) row) == ref)

    feval header (Lt colname ref) = \row -> ((get_kth_elem (get_column_index
     colname header) row) < ref)

    feval header (Gt colname ref) = \row -> ((get_kth_elem (get_column_index
     colname header) row) > ref)

    feval header (In colname list) = \row -> (elem (get_kth_elem
     (get_column_index colname header) row) list)

    feval header (FNot cond) = \row -> (not $ (feval header cond) row)

    feval header (FieldEq colname1 colname2) = \row -> ((get_kth_elem (get_column_index
     colname1 header) row) == (get_kth_elem (get_column_index colname2 header) row))

-- 3.3
create_graph :: (Row -> Row -> (Maybe Value)) -> Table -> Table
create_graph edgeop table = ["From", "To", "Value"] : (create_graph_aux edgeop
 (head $ tail table) (tail $ tail table))

create_graph_aux edgeop row [] = []
create_graph_aux edgeop row table = (filter (\x -> x /= []) $ map map_op table)
 ++ (create_graph_aux edgeop (head table) (tail table))
    where
        map_op aux_row = eval_op (edgeop row aux_row)
            where
                eval_op (Just value)
                    | (head row) < (head aux_row) = [(head row), (head aux_row), value]
                    | otherwise = [(head aux_row), (head row), value]
                eval_op (Nothing) = []

-- 3.4
similarities_query = Filter (Gt "Value" (4.0 :: Float)) (Sort "Value" (Graph similarities_op
 (FromCSV lecture_grades_csv)))

similarities_op :: Row -> Row -> (Maybe Value)
similarities_op row1 row2
    | (head row1) == "" || (head row2) == "" = Nothing
    | otherwise = Just $ show $ sum (zipWith is_equal (tail row1) (tail row2))
    where
        is_equal :: Value -> Value -> Int
        is_equal a b
            | (a == b) = 1
            | otherwise = 0

{-
    TASK SET 4
-}

subsir list1 list2 = head $ foldr(\myList -> map head . scanr1 myFunc . zipWith
 (\x y -> [x,y]) myList) gol mapped
    where
        mapped = map (\x -> flip (++) [[]] $ map (\y -> [x | x == y]) list2) list1
        gol = take (length list2) (repeat [])
        myFunc [a , b] [c , d] 
         | ((length a) == 0) = (max_len b c) : [b]
         | otherwise = (a ++ d) : [b]
            where
                max_len l1 l2
                    | (length l1) > (length l2) = l1
                    | otherwise = l2

correct_name :: String -> [String] -> String -> String -> String
correct_name act [] max real = real
correct_name act (x:xs) max real
    | (length (subsir act x)) > (length max) = correct_name act xs (subsir act x) x
    | otherwise = correct_name act xs max real

correct_table :: String -> CSV -> CSV -> CSV
correct_table colname email_map_csv hw_grades_csv = write_csv (hunion (get_left_columns
 colname email_map_csv) (hunion ([colname] : (map (\x -> [x]) (f colname email_map_csv
 hw_grades_csv))) (get_right_columns colname email_map_csv)))
    
    where
        get_right_columns :: String -> CSV -> Table
        get_right_columns colname csv = getColumnsInterval (read_csv email_map_csv)
            ((get_column_index colname (head $ read_csv email_map_csv)) + 1) (length
             $ head $ read_csv email_map_csv)

        get_left_columns :: String -> CSV -> Table
        get_left_columns colname csv = getColumnsInterval (read_csv email_map_csv)
             1 ((get_column_index colname (head $ read_csv email_map_csv)) - 1)

        f colname email_map_csv hw_grades_csv = map (\x -> correct_name x (get_value_list
         hw_grades_csv) "" "") (get_value_list email_map_csv)
            where
                get_value_list table = as_list colname (projection [colname] (read_csv table))   

get_lecture_grade :: Row -> Row
get_lecture_grade row = (head row) : (printf "%.2f" $ (sum (map evalStringToFloat
 (tail row))) / 14) : []

get_parcurs_grade :: Row -> Row
get_parcurs_grade row = (head row) : (printf "%.2f" $ sum (map evalStringToFloat
 (tail row))) : []

grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades email_map_csv hw_grades_csv exam_grades_csv lecture_grades_csv = write_csv
  (["Nume","Punctaj Teme", "Punctaj Curs","Punctaj Exam","Punctaj Total"] : (map repair
  (filter (\x -> (head x) /= "Nume") (tjoin "Nume" (tjoin "Email" (tjoin "Nume"
  email_table hw_table) lecture_table) exam_table))))

    where
        repair row = (head row) : (tail $ tail $ row) ++ [printf "%.2f" (get_final_grade row)]
            where
                get_final_grade row
                    | (evalStringToFloat (get_kth_elem 3 row) + evalStringToFloat
                      (get_kth_elem 4 row) < 2.5) = 4.0

                    | evalStringToFloat (get_kth_elem 5 row) < 2.5 = 4.0
                    
                    | otherwise = (min (evalStringToFloat (get_kth_elem 3 row) +
                      evalStringToFloat (get_kth_elem
                     4 row)) 5) + evalStringToFloat (get_kth_elem 5 row)

        myFunc row = (head row) : (get_value (head $ tail row) lecture_table)

        get_value value table
            | length (filter (\r -> (head r) == (value)) table) == 0 = [""]
            | otherwise = head (filter (\r -> (head r) == (value)) table)

        email_table = ["Nume", "Email"] : sortBy (\x y -> if (head x) < (head y) then LT
         else GT) (read_csv $ (correct_table "Nume" email_map_csv hw_grades_csv))

        hw_table = rmap get_parcurs_grade ["Nume", "Total"] (read_csv hw_grades_csv)

        exam_table = compute_exam_grades (read_csv exam_grades_csv)

        lecture_table = rmap get_lecture_grade ["Email", "Lecture"] (read_csv lecture_grades_csv)