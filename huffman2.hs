import Data.List

-- Data types 
data Tree = Leaf Char Int | Node [Tree] Int deriving Show

-- Defining instances of Eq and Ord so I can compare Nodes and Leaves with each other based on their frequency
instance Eq Tree where 
    (Leaf _ f1)   == (Leaf _ f2)   = f1 == f2 
    (Node _ f1) == (Node _ f2) = f1 == f2 

instance Ord Tree where 
    compare (Leaf _ f1)   (Leaf _ f2)   = compare f1 f2 
    compare (Node _ f1) (Node _ f2) = compare f1 f2 
    compare (Node _ f1) (Leaf _ f2)   = compare f1 f2 
    compare (Leaf _ f1)   (Node _ f2) = compare f1 f2 

-- Utility functions 
key (k, v) = k 
value (k, v)= v
frequency (Leaf _ f) = f 
frequency (Node _ f) = f 
sum_frequency x y = (frequency x) + (frequency y)

set_minus lst1 lst2 = set_minus_helper lst1 lst2 []
set_minus_helper [] lst acc = acc 
set_minus_helper lst [] acc = acc ++ lst 
set_minus_helper (x:xs) (y:ys) acc = 
    if x == y then set_minus_helper xs ys acc 
    else set_minus_helper xs ys (acc ++ [x])

sum_frequencies [] = 0
sum_frequencies lst =
    let frequencies = [f | f <- (map (\x -> frequency x) lst) ] in
        sum frequencies
    

element_of element [] = False
element_of element (x:xs) = 
    if element == x then True 
    else element_of element xs 

lookup_value :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup_value key [] = Nothing 
lookup_value key ((k, v):lst) = 
    if key == k then Just v 
    else lookup_value key lst

-- Takes a key-value pair and an association list. 
-- If the key is found, returns the association list with the sum of the old and new value for that key. 
-- Otherwise, the association is returned with the new key-value pair appended to it. 
add_or_increment :: (Eq k, Num v) => (k, v) -> [(k, v)] -> [(k, v)]
add_or_increment (k, v) [] = [(k, v)]
add_or_increment (k, v) (x:xs) = 
    let k1 = key x 
        v1 = value x in 
        if k == k1 then [(k, v1 + v)] ++ xs
        else [x] ++ add_or_increment (k, v) xs 


-- Returns an association list of characters and their frequency as integers. 
count_char_frequency :: String -> [(Char, Int)]
count_char_frequency str = count_char_frequency_helper str []
count_char_frequency_helper [] acc = acc 
count_char_frequency_helper (x:xs) acc = 
    count_char_frequency_helper xs (add_or_increment (x, 1) acc)



-- Converts an association list to a list of leaves 
make_leaves :: [(Char, Int)] -> [(Tree)]
make_leaves lst = [(Leaf k v) | (k,v) <- lst]



-- Insert an element into a sorted list so that the resulting list is sorted (ascending)
sorted_insert :: (Eq a, Ord a) => a -> [a] -> [a]
sorted_insert element lst = sorted_insert_helper element lst []

sorted_insert_helper element [] acc = acc ++ [element]
sorted_insert_helper element (x:xs) acc = 
    if element <= x then acc ++ [element, x] ++ xs 
    else sorted_insert_helper element xs (acc ++ [x])

sorted_insert_many [] lst = lst 
sorted_insert_many (x:xs) lst = 
    sorted_insert_many xs (sorted_insert x lst)


make_tree :: [Tree] -> Int -> Maybe Tree 
make_tree leaf_list n = 
    -- First, sort the list. Do this only once.
    -- Until there are only two elements in the list, do the following: 
    -- 1. Take the two first elements of the sorted list (the two smallest ones) 
    -- 2. Make a node of them. The frequency of this node is the sum of the two. 
    -- 3. Insert the resulting node in the list, making sure it is still sorted.
    -- 4. Repeat.
    -- When there are only two elements left, combine them and return the resulting node.
    let sorted_leaf_list = sort leaf_list in 
        make_tree_helper sorted_leaf_list n
    
-- TODO: Make trees maybe heheheheh
make_tree_helper :: [Tree] -> Int -> Maybe Tree
make_tree_helper [] n = Nothing
make_tree_helper [l1] n = Just l1
make_tree_helper tree_list n = 
    let tree_nodes = take n tree_list
        new_node = (Node tree_nodes (sum_frequencies tree_nodes))
        remaining_list = set_minus tree_list tree_nodes in 
        make_tree_helper (sorted_insert new_node remaining_list) n


-- Use depth first to traverse the tree, adding 0's and 1's for every left and right 
-- turn, respectively. When a leaf is encountered, add (letter, encoding) to the accumulator. 
get_encoding_dict :: Maybe Tree -> [(Char, [Int])]
get_encoding_dict tree = 
    case tree of 
        Nothing -> []
        Just t -> get_encoding_dict_helper t [] [] 0

get_encoding_dict_helper :: Tree -> [Int] -> [(Char, [Int])] -> Int -> [(Char, [Int])]
get_encoding_dict_helper (Leaf letter _) encoding acc n = 
    [(letter, encoding)] ++ acc 

get_encoding_dict_helper (Node (branch:branches) f) encoding acc n = 
    let leftmost_acc = get_encoding_dict_helper branch (encoding ++ [n]) acc 0 in 
        get_encoding_dict_helper (Node branches f) encoding (acc ++ leftmost_acc) (n + 1) 

get_encoding_dict_helper (Node [] f) encoding acc n = acc


get_encoded_message_from_dict :: String -> [(Char, [Int])] -> [Int]
get_encoded_message_from_dict [] encoding_dict = []
get_encoded_message_from_dict [x] encoding_dict = 
    let encoding = lookup_value x encoding_dict in 
        case encoding of 
            Just enc -> enc 
            Nothing -> error "Could not find an encoding for the symbol: " x

get_encoded_message_from_dict (x:xs) encoding_dict = 
    let encoding = lookup_value x encoding_dict in 
        case encoding of
            Just enc -> enc ++ get_encoded_message_from_dict xs encoding_dict
            Nothing -> error "Could not find an encoding for the symbol: " x

get_encoded_message :: String -> Int -> [Int]
get_encoded_message msg n = 
    let freq = count_char_frequency msg 
        leaves = make_leaves freq 
        tree = make_tree leaves n
        dict = get_encoding_dict tree in 
            get_encoded_message_from_dict msg dict 


get_huffman_tree :: String -> Int -> Tree
get_huffman_tree str n = 
    let freq = count_char_frequency str 
        tree = make_tree (make_leaves freq) n in 
            case tree of 
                Nothing -> error "Could not generate huffman tree for an empty message. " 
                Just t -> t 


decode_bit_stream :: [Int] -> Tree -> String 
decode_bit_stream stream huffman_tree = traverse_tree_from_bit_stream stream huffman_tree huffman_tree ""

some_tree = get_huffman_tree "hello there" 5

traverse_tree_from_bit_stream :: [Int] -> Tree -> Tree -> String -> String
traverse_tree_from_bit_stream [] huffman_tree root msg = 
    let Leaf letter _ = huffman_tree in 
            msg ++ [letter]

traverse_tree_from_bit_stream (x:xs) huffman_tree root msg =
    case huffman_tree of 
        -- Take the left node if encountering a 0, right node otherwise.
        Node branches _ -> traverse_tree_from_bit_stream xs (branches!!x) root msg 

        -- If it's a leaf, add the corresponding letter to the message and start again from the root. 
        Leaf char _  -> let bit_stream = ([x] ++ xs) 
                            decoded_message = (msg ++ [char]) in
                                traverse_tree_from_bit_stream bit_stream root root decoded_message

                            



-- Run IO 
run_encode = do 
    putStrLn "Please enter a message to encode: "
    user_input <- getLine 
    putStrLn "... and the arity of the encoding alphabet: "
    n_str <- getLine 
    let n = read n_str :: Int
    let encoded_message = get_encoded_message user_input n

    putStrLn "The encoded message is: "
    return encoded_message

run_decode = do 
    putStrLn "Please enter an encoded message :: [Int] to decode: "
    user_input <- getLine
    let u_input = read user_input :: [Int]
    putStrLn "Decoding message with proprietary and copyrighted n-ary Huffman tree..."
    let msg = decode_bit_stream u_input some_tree
    putStrLn "The decoded message is: "
    return msg
