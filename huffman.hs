import Data.List

-- Data types 
data Tree = Leaf Char Integer | Node Tree Tree Integer deriving Show

-- Defining instances of Eq and Ord so I can compare Nodes and Leaves with each other based on their frequency
instance Eq Tree where 
    (Leaf _ f1) == (Leaf _ f2) = f1 == f2 
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
count_char_frequency :: String -> [(Char, Integer)]
count_char_frequency str = count_char_frequency_helper str []
count_char_frequency_helper [] acc = acc 
count_char_frequency_helper (x:xs) acc = 
    count_char_frequency_helper xs (add_or_increment (x, 1) acc)


-- Converts an association list to a list of leaves 
make_leaves :: [(Char, Integer)] -> [(Tree)]
make_leaves [] = []
make_leaves (x:xs) = 
    let k = key x 
        v = value x in 
            [(Leaf k v)] ++ make_leaves xs

-- Insert an element into a sorted list so that the resulting list is sorted (ascending)
sorted_insert :: (Eq a, Ord a) => a -> [a] -> [a]
sorted_insert element lst = sorted_insert_helper element lst []

sorted_insert_helper element [] acc = acc ++ [element]
sorted_insert_helper element (x:xs) acc = 
    if element < x then acc ++ [element, x] ++ xs 
    else sorted_insert_helper element xs (acc ++ [x])

make_tree :: [Tree] -> Tree 
make_tree leaf_list = 
    -- First, sort the list. Do this only once.
    -- Until there are only two elements in the list, do the following: 
    -- 1. Take the two first elements of the sorted list (the two smallest ones) 
    -- 2. Make a node of them. The frequency of this node is the sum of the two. 
    -- 3. Insert the resulting node in the list, making sure it is still sorted.
    -- 4. Repeat.
    -- When there are only two elements left, combine them and return the resulting node.
    let sorted_leaf_list = sort leaf_list in 
        make_tree_helper sorted_leaf_list

make_tree_helper :: [Tree] -> Tree
make_tree_helper [l1, l2] = Node l1 l2 (sum_frequency l1 l2)

make_tree_helper (l1 : l2 : leaves) = 
    let node = Node l1 l2 (sum_frequency l1 l2) in 
        make_tree_helper (sorted_insert node leaves)

-- Use depth first to traverse the tree, adding 0's and 1's for every left and right 
-- turn, respectively. When a leaf is encountered, add (letter, encoding) to the accumulator. 
get_encoding_dict :: Tree -> [(Char, [Bool])]
get_encoding_dict tree = get_encoding_dict_helper tree [] []

get_encoding_dict_helper :: Tree -> [Bool] -> [(Char, [Bool])] -> [(Char, [Bool])]
get_encoding_dict_helper (Leaf letter _) encoding acc = 
    [(letter, encoding)] ++ acc 

get_encoding_dict_helper (Node l1 l2 _) encoding acc = 
    -- Left first
    let left_acc = get_encoding_dict_helper l1 (encoding ++ [False]) acc in 
        -- Then take the right
        get_encoding_dict_helper l2 (encoding ++ [True]) left_acc

get_encoded_message_from_dict :: String -> [(Char, [Bool])] -> [Bool]
get_encoded_message_from_dict [x] encoding_dict = 
    let encoding = lookup_value x encoding_dict in 
        case encoding of 
            Just enc -> enc 
            Nothing -> error "Could not find an encoding for the letter: " x

get_encoded_message_from_dict (x:xs) encoding_dict = 
    let encoding = lookup_value x encoding_dict in 
        case encoding of
            Just enc -> enc ++ get_encoded_message_from_dict xs encoding_dict
            Nothing -> error "Could not find an encoding for the letter: " x

get_encoded_message :: String -> [Bool]
get_encoded_message msg = 
    let freq = count_char_frequency msg 
        leaves = make_leaves freq 
        tree = make_tree leaves 
        dict = get_encoding_dict tree in 
            get_encoded_message_from_dict msg dict 

get_huffman_tree :: String -> Tree
get_huffman_tree str = 
    let freq = count_char_frequency str 
        leaves = make_leaves freq in 
            make_tree leaves

decode_bit_stream :: [Bool] -> Tree -> String 
decode_bit_stream stream huffman_tree = traverse_tree_from_bit_stream stream huffman_tree huffman_tree ""

some_tree = get_huffman_tree "mississippi river is a nice river"

traverse_tree_from_bit_stream :: [Bool] -> Tree -> Tree -> String -> String
traverse_tree_from_bit_stream [] huffman_tree root msg = 
    let Leaf letter _ = huffman_tree in 
            msg ++ [letter]

traverse_tree_from_bit_stream (x:xs) huffman_tree root msg =
    case huffman_tree of 
        -- Take the left node if encountering a 0, right node otherwise.
        Node l1 l2 _ -> if x == False then traverse_tree_from_bit_stream xs l1 root msg
                        else             traverse_tree_from_bit_stream xs l2 root msg

        -- If it's a leaf, add the corresponding letter to the message and start again from the root. 
        Leaf char _  -> let bit_stream = ([x] ++ xs) 
                            decoded_message = (msg ++ [char]) in
                                traverse_tree_from_bit_stream bit_stream root root decoded_message
    



