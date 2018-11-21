import Data.List

-- Data types 
data Tree = Leaf Char Int | Node Tree Tree Int deriving Show
data Bit  = One | Zero deriving (Show, Eq, Read)

-- Defining instances of Eq and Ord so I can compare Nodes and Leaves with each other based on their frequency
instance Eq Tree where 
    (Leaf _ f1)   == (Leaf _ f2)   = f1 == f2 
    (Node _ _ f1) == (Node _ _ f2) = f1 == f2 

instance Ord Tree where 
    compare (Leaf _ f1)   (Leaf _ f2)   = compare f1 f2 
    compare (Node _ _ f1) (Node _ _ f2) = compare f1 f2 
    compare (Node _ _ f1) (Leaf _ f2)   = compare f1 f2 
    compare (Leaf _ f1)   (Node _ _ f2) = compare f1 f2 

-- Utility functions 
key (k, v) = k 
value (k, v)= v
frequency (Leaf _ f) = f 
frequency (Node _ _ f) = f 
sum_frequency x y = (frequency x) + (frequency y)

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


make_tree :: [Tree] -> Maybe Tree 
make_tree leaf_list = 
    -- First, sort the list. Do this only once.
    -- Until there are only two elements in the list, do the following: 
    -- 1. Take the two first elements of the sorted list (the two smallest ones) 
    -- 2. Make a node over both of them. The frequency of this node is the sum of the two. 
    -- 3. Insert the resulting node in the list, making sure it is still sorted.
    -- 4. Repeat.
    -- When there is just one node left in the list, return that as the root of the tree.
    let sorted_leaf_list = sort leaf_list in 
        make_tree_helper sorted_leaf_list
    

make_tree_helper :: [Tree] -> Maybe Tree
make_tree_helper [] = Nothing
make_tree_helper [l] = Just l
make_tree_helper (l1 : l2 : leaves) = 
    let node = Node l1 l2 (sum_frequency l1 l2) in 
        make_tree_helper (sorted_insert node leaves)


-- Use depth first to traverse the tree, adding 0's and 1's to the encoding for every left and right 
-- turn, respectively. When a leaf is encountered, add (letter, encoding) to the accumulator. 
get_encoding_dict :: Maybe Tree -> [(Char, [Bit])]
get_encoding_dict tree = 
    case tree of 
        Nothing -> []
        Just t -> get_encoding_dict_helper t [] []

get_encoding_dict_helper :: Tree -> [Bit] -> [(Char, [Bit])] -> [(Char, [Bit])]
get_encoding_dict_helper (Leaf letter _) encoding acc = 
    [(letter, encoding)] ++ acc 

get_encoding_dict_helper (Node l1 l2 _) encoding acc = 
    -- Left first
    let left_acc = get_encoding_dict_helper l1 (encoding ++ [Zero]) acc in 
        -- Then take the right
        get_encoding_dict_helper l2 (encoding ++ [One]) left_acc


-- Given an dictionary/association list of (char, bit sequence), converts
-- a string into its corresponding bit sequence.
get_encoded_message_from_dict :: String -> [(Char, [Bit])] -> [Bit]
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

-- Given a string, returns the compressed string constructed from the
-- optimal prefix code for the string.
get_encoded_message :: String -> [Bit]
get_encoded_message msg = 
    let freq = count_char_frequency msg 
        leaves = make_leaves freq 
        tree = make_tree leaves 
        dict = get_encoding_dict tree in 
            get_encoded_message_from_dict msg dict 

-- Returns the dictionary/association list that represents the optimal
-- prefix code for a string.
get_encoding_dict_from_string :: String -> [(Char, [Bit])]
get_encoding_dict_from_string msg = 
    let freq = count_char_frequency msg 
        leaves = make_leaves freq 
        tree = make_tree leaves in
        get_encoding_dict tree 

-- Given a string, returns the binary tree from which the optimal prefix
-- code can be derived.
get_huffman_tree :: String -> Tree
get_huffman_tree str = 
    let freq = count_char_frequency str 
        tree = make_tree (make_leaves freq) in 
            case tree of 
                Nothing -> error "Could not generate huffman tree for an empty message. " 
                Just t -> t 

-- Given a bit stream and a binary tree from which to decode the string, 
-- returns the decoded string. 
decode_bit_stream :: [Bit] -> Tree -> String 
decode_bit_stream stream huffman_tree = 
    case huffman_tree of 
        Node _ _ _ -> traverse_tree_from_bit_stream stream huffman_tree huffman_tree ""
        Leaf c f   -> replicate f c -- Edge case where the tree was constructed
                                    -- From a message with only one unique character.
    
-- Binary tree used for decoding strings in run_decode.
mississippi_tree :: Tree
mississippi_tree = get_huffman_tree "mississippi river"

-- Given a bit stream and a binary tree, traverses the tree by reading 
-- bits from the bit stream in sequence. When a leaf is encountered, the
-- corresponding letter is added to the decoded message and the procedure
-- Continues from the root.
traverse_tree_from_bit_stream :: [Bit] -> Tree -> Tree -> String -> String
traverse_tree_from_bit_stream [] huffman_tree root msg = 
    case huffman_tree of 
        Leaf letter _ -> msg ++ [letter]
        Node _ _ _    -> error "Bit stream ended, but traversal did not end at leaf node. Was the bit stream decoded with the right tree?"


traverse_tree_from_bit_stream (x:xs) huffman_tree root msg =
    case huffman_tree of 
        -- Take the left node if encountering a 0, right node otherwise.
        Node l1 l2 _ -> if x == Zero then traverse_tree_from_bit_stream xs l1 root msg
                        else              traverse_tree_from_bit_stream xs l2 root msg

        -- If it's a leaf, add the corresponding letter to the message and start again from the root. 
        Leaf char _  -> let bit_stream = ([x] ++ xs) 
                            decoded_message = (msg ++ [char]) in
                                traverse_tree_from_bit_stream bit_stream root root decoded_message

                            
-- Simple converters
bit_string_to_bit_arr :: String -> [Bit]
bit_string_to_bit_arr str = [bit | c <- str, let bit = if c == '0' then Zero else One]


bit_arr_to_bit_string :: [Bit] -> String
bit_arr_to_bit_string arr = [char | b <- arr, let char = if b == Zero then '0' else '1']


-- Demo IO
run_encode = do 
    putStrLn "Please enter a message to encode: "
    user_input <- getLine 
    putStrLn "The encoded message is: "
    let encoded_message = get_encoded_message user_input
    return encoded_message

run_decode = do 
    putStrLn "Please enter a bit stream :: [Bit] to decode with our proprietary Huffman coding scheme: "
    user_input <- getLine 
    let decoded_message = read user_input :: [Bit]
    return (decode_bit_stream decoded_message mississippi_tree)

