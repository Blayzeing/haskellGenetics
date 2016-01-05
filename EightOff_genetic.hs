{- Implemention for use by genetic algorithm to find optimum play strategy.
 - This system requires a genetic sequence of length 10.
 -}
module EightOff_genetic where -- Define the module
import System.Random -- Import System.Random for shuffling
import Data.List -- Import Data.List to gain access to `sortBy` and `delete`
import Debug.Trace
import Evolution

-- Define Suit and Pip algebraic types to use in the Card type:
data Suit = Spades | Hearts | Diamonds | Clubs
            deriving (Eq, Enum, Show, Ord) -- Derive from Eq, Enum and show to allow for equallity checking, successor and predeccessor references and display
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
            deriving (Eq, Ord, Enum, Show) -- Also derive from ord so I can use the `max` function (ORd is derived above for testing purposes)

-- Define the datatypes needed:
type Card = (Pip, Suit) -- A tuple of a Pip and a Suit
type Deck = [Card] -- A deck is a list of Cards
type Foundations = [Card] -- Foundations can be a list of Cards also, only ever holding the top card of each pile
type Columns = [Deck] -- Columns is a list of list of Cards, or a list of non-full Decks
type Reserve = [Card] -- Reserve is simply a list of Cards that should always be of length <= 8
type EOBoard = (Foundations, Columns, Reserve) -- EOBoard is a 3-tuple that represents a solitaire board

-- The pack constant constructs a full deck of 52 Cards using a list comprehension to form a list of every combination of Pip and Suit
pack :: Deck
pack = [(p,s) | s <- [Spades ..], p <- [Ace ..]]

-- isKing returns the comparison of the pip of a given card against King, taking a Card and returning true if it's Pip value is equivelent to King, and false if not.
isKing :: Card -> Bool
isKing (p,_) = p == King

-- isAce returns the comparison of the pip of a given card against Ace, taking a Card and returning true if it's Pip value is equivelent to Ace, and false if not.
isAce :: Card -> Bool
isAce (p,_) = p == Ace

-- sCard returns the successor card of a given card. It does this by returning a new Card constructed with the same suit, but successor Pip to the given Card.
sCard :: Card -> Card
sCard card@(p, s) = (succ p, s)

-- pCard returns the predeccessor card of a given card. It does this by returning a new Card constructed with the same suit, but predeccessor Pip to the given Card.
pCard :: Card -> Card
pCard card@(p, s) = (pred p, s)

-- shuffle takes an integer and returns a randomly shuffled full deck of Cards
shuffle :: Int -> Deck
shuffle seed = map snd randomized -- map the second element of the randomly-ordered list of cards into it's own list and return it
    where
        zipped = (zip (take 52 (randoms (mkStdGen seed):: [Int])) pack ) -- Take 52 random ints and zip them to each card in a pack
        randomized = sortBy (\(n1,_) (n2,_) -> compare n1 n2) zipped -- Order the zipped list based on it's random numbers, randomizing the Card order

-- eODeal takes a seed (int) and returns an EOBoard containing the Cards in from that Deck
eODeal :: Int -> EOBoard
eODeal seed = ([], cols, res) -- Construct the board with an empty foundations and use the variables defined below to place the deck's cards.
    where
        deck = shuffle seed -- Creates a randomly shuffled (based on the given seed) deck to deal.
        -- The comprehension below splits the first 48 cards of a deck into 8 6-long columns, by taking every card up to the end of,
        -- then dropping every card before the beginning of each column and forming a list:
        cols = [drop (n*6) (take (n*6 + 6) deck) | n <- [0..7]]
        res = drop 48 deck -- Finally, take the final 4 cards from the deck to form the cards in the reserves

-- toFoundations is an autoplay function that takes an EOBoard and plays every possible move that results in a card being added to the foundations, returning a new EOBoard.
-- On each iteration ('step') of game-play, the function generates a list of cards needed to progress the game, then attempts to find and move those cards into foundations,
-- stopping when no changes occur to the board after a step, in other words, when none of the desired cards are avaialble to be moved.
toFoundations :: EOBoard -> EOBoard
toFoundations board
    | board == nextBoard = board -- If the next step is the same as this one then no further actions can be performed, so just return the board, the sequence has ended
    | otherwise = toFoundations nextBoard -- If not, then a new action can be performed, so use the performed action and continue on to the next step recursively
    where nextBoard = playMoves (getRequiredCards board) board -- Define the next step as the board state when required cards have attempted to be moved
    
-- getRequiredCards returns a list of cards that can forward the game (namely, the successors to the cards in the foundations after taking an EOBoard)
getRequiredCards :: EOBoard -> [Card]
-- The function below folds the nextCards into the baseCards using the `addToFoundations` function on each card within nextCards, adding it to the baseCards
getRequiredCards board@(founds,_,_) = foldr (\ card rest -> addToFoundations rest card) baseCards nextCards
    where
        nextCards = map sCard (filter (not . isKing) founds) -- The successors of all non-king cards within foundations
        baseCards = [(p,s) | p <- [Ace], s <- [Spades ..]] -- Basic needed cards, assuming an empty foundations

-- playMoves takes a list of Cards (the required moves) and an EOBoard, then attempts to make those moves by finding those cards and placing them into foundations
playMoves :: [Card] -> EOBoard -> EOBoard
playMoves [] board = board -- If there are no moves to make, return the board in it's current state.
playMoves neededCards@(neededCard:rest) board@(founds,cols,res) -- use AS notation to split the board and card list into useful sub-variables
    -- If the needed card is in the reserves, then remove it from there and place it in the foundations, then recurse through the other needed cards:
    | elem neededCard res = playMoves rest (addToFoundations founds neededCard, cleanCols, delete neededCard res)
    -- If the needed card is in the cols (heads), then remove it from there and place it in the foundations, then recurse through the other needed cards:
    | elem neededCard (map head cleanCols) = playMoves rest (addToFoundations founds neededCard, map (delete neededCard) cleanCols, res)
    -- If the neededCard was not found anywhere, remove it from the list of needed cards and try to find the next card with no changes to the board:
    | otherwise = playMoves rest board
    where cleanCols = filter (not . null) cols -- Needed to remove empty columns, so a head function is not called on one.

-- addToFoundations adds a given card to a foundations-formatted collection of cards, raising the suit's pip value if it's there, or adding it if not.
addToFoundations :: Foundations -> Card -> Foundations
addToFoundations [] c = [c] -- Base case is to add this card if no other cards of the same suit are found
addToFoundations (topCard@(fPip, fSuit):fTail) card@(cPip, cSuit) -- Use AS notation to split up the arguments
    | fSuit == cSuit = (max fPip cPip, cSuit):fTail -- If the Foundations contain a card with the same Suit as the card being added, raise the Pip of that card
    | otherwise = topCard:(addToFoundations fTail card) -- If not, then try to add the new card (`card`) to the rest of the list.

-- findMoves takes an EOBoard and returns a list of sensible potential moves, ordered by how likely they are to progress the game
findMoves :: [Double] -> EOBoard -> [EOBoard]
findMoves gene board@(f, cols, res) = map fst (sortBy (\(_,n1) (_,n2) -> compare n1 n2) unsortedMoves) -- Sort all potential moves by rank
    where
        -- Construct a list of cards that can be appended to the columns (predecessors to the heads of each column):
        neededCards = map pCard (filter (not.isAce) (map head (filter (not . null) cols)))
        -- Construct a list of king cards that are in the columns, but not already at column bases:
        neededKings = ([(King, s) | s <- [Spades ..]] \\ f) \\ (map (head.reverse) (filter (not.null) cols))
        unsortedMoves = map resMaybe $ filter isJust $ map (getSequenceMove gene board) (neededCards ++ neededKings) -- List of possible moves, with importance rank

-- getSequenceMove takes an EOBoard and a single card that is wanted to progress the game and may return a ranked new board position (move)
getSequenceMove :: [Double] -> EOBoard -> Card -> Maybe (EOBoard, Double)
getSequenceMove gene board@(f, cols, res) card
    | isKing card && length cols == 8 = Nothing -- If the card is a king, only proceed if there is somewhere to put the new column it will form
    | elem card res = Just ((f, addToColumns cols [card], delete card res), gene!!0 - kValue - bValue) -- The needed card is in the reserves, pull it out and add it to the columns
    | null columns = Nothing -- The needed card exists in no columns or reserves, therefore there is no move to get it into a column
    -- If not, the needed card must be in a column:
    -- If the sequence goes all the way to the head of the column and it is short enough, then remove it from it's column and add it to the correct column:
    | length headToCard == length seq && length seq <= resSpace + 1 = Just ((f, addToColumns (map (\\ seq) cols) seq, res), (gene!!1) * fromIntegral(length seq) - kValue - hValue - gene!!6)
    -- If the sequence is too long, but still goes to the head of it's column, don't even try to move it, that would create a loop:
    | length headToCard == length seq = Nothing
    -- If it's in a column but covered, uncover it (if there's a free res slot and no chain is broken) and give it a cost as the number of moves it would take to uncover it:
    | length coveringSeq <= resSpace = Just ((f, map (\\ coveringSeq) cols, res ++ coveringSeq), (gene!!2) * fromIntegral(length headToCard - length seq) - kValue - hValue)
    -- If none of the above, then it's in a column, but there are no moves to put it anywhere, so return Nothing.
    | otherwise = Nothing
    where
        bonusCards = getRequiredCards board -- These cards will progress the game, so are worth more if found
        hValue = if elem (head seq) bonusCards then gene!!3 else gene!!7 -- Rank modifier for bonus cards (at the end of sequences)
        bValue = if elem card bonusCards then gene!!4 else gene!!8 -- Rank modifier for bonus cards (singles)
        kValue = if isKing card then gene!!5 else gene!!9 -- Rank modifier for creation of a new column (king moves)
        columns = filter (elem card) cols -- The needed cards' column[s]
        column = head columns -- The needed card's column
        topCard = head column -- The top card on the needed cards' column
        headToCard = getTillCard column card -- Every card upto and including the needed card
        seq = reverse (getPredSequence (reverse headToCard)) -- The sequence of cards to be placed, the head being the lowest in sequence
        resSpace = 8 - length res -- The amount of space left in the reserves
        coveringSeq = getSuccSequence headToCard -- This will store the succession sequence of cards closest to the columns' head
        nonBroken = isKing topCard || (sCard topCard /= head (tail column)) -- Boolean check to see if removing the top card would break a pre-existing sequence

-- getTillCard takes a list of cards and returns all cards before and including a given card (returns entire list if card not found)
getTillCard :: [Card] -> Card -> [Card]
getTillCard [] c = [] -- If no cards are given, return a list of none
getTillCard (h:t) c
    | h == c = [c] -- If the head of the list is the wanted card, return a list of just that card
    | otherwise = h:(getTillCard t c) -- Else, prepend the current head to the list constructed by calling getTillCard on the tail

-- getPredSequence takes a list of cards and returns all cards (starting from the head) that are predecessors of the previous card
getPredSequence :: [Card] -> [Card]
getPredSequence [] = [] -- If no list was given, return a null list
getPredSequence (h:t)
    | null t || isAce h = [h] -- If there is no next card, or the next card cannot be a predecessor because the current head is an ace, return just the head
    | head t == pCard h = h:(getPredSequence t) -- If the head of the tail of the list is the predecessor of this card, append this head and run recursively
    | otherwise = [h] -- Otherwise, just return the current head in it's own list

-- getSuccSequence takes a list of cards and returns all cards (starting from the head) that are successors of the previous card
getSuccSequence :: [Card] -> [Card]
getSuccSequence [] = [] -- If no list was given, return a null list
getSuccSequence (h:t)
    | null t || isKing h = [h] -- If there is no next card, or the next card cannot be a successor because the current head is a king, return just the head
    | head t == sCard h = h:(getSuccSequence t) -- If the head of the tail of the list is the successor of this card, append this head and run recursively
    | otherwise = [h] -- Otherwise, just return the current head in it's own list


-- Inserts a card sequence at the correct place in a Columns
addToColumns :: Columns -> [Card] -> Columns
addToColumns cols seq
    | isKing (head (reverse seq)) = seq:populatedCols -- If the sequence's base is a king, simply create a new column
    | otherwise = map (insertSequence seq) populatedCols -- If not, then attempt to prepend the sequence to each column
    where
        populatedCols = filter (not . null) cols -- Get rid of null columns
        insertSequence :: [Card] -> [Card] -> [Card]
        -- insertSequence prepends a sequence to a column if it fits sequentially:
        insertSequence sequence column@(ch:ct)
            | null sequence || isAce ch = column -- If the column ends on an Ace, or if the sequence is null, do nothing.
            | pCard ch == head (reverse sequence) = sequence ++ column -- If the sequence matches, prepend it
            | otherwise = column -- If it does not, then also do nothing.

-- ChooseMove uses findMoves to generate a list of moves, then chooses the best one.
-- As findMoves already ranks the moves, chooseMoves only needs to select the head or return Nothing if there is no head
chooseMove :: [Double] -> EOBoard -> Maybe EOBoard
chooseMove gene board
    | null moves = Nothing
    | otherwise = Just (toFoundations (head moves))
    where moves = findMoves gene board -- The list of all moves

-- eOGame takes an initial board, then plays the game, returning a final score
eOGame :: [Double] -> EOBoard -> Int
eOGame gene board
    | isJust nextMove = eOGame gene (resMaybe nextMove) -- If there is a next move, play it with the updated board state
    | otherwise = score (toFoundations board) -- There is no next move, just return this board's score after running toFoundations a final time
    where nextMove = chooseMove gene board -- Calculate the next board state

-- Returns a tuple consisting of the number of wins in 100 games and the average score
eOExpt :: Int -> [Double] -> (Int, Double)
eOExpt seed gene = eOExptNum seed gene 100

eOExptSingle :: Int -> [Double] -> Double
eOExptSingle seed gene = fromIntegral $ fst $ eOExptNum seed gene 100

eOExptNum :: Int -> [Double] -> Int -> (Int, Double)
eOExptNum seed gene games = (winCount, average) -- Return the appropriate information
    where
        seeds = randoms (mkStdGen seed) :: [Int]
        scores = [eOGame gene ( eODeal x) | x <- take games seeds] -- Store all game outcomes
        winCount = length $ filter (== 52) scores -- Find the number of won games
        average = (fromIntegral (foldr (+) 0 scores)) / (fromIntegral games) -- Calculate the average score

-- Calculate the score of a given EOBoard, this is done by subtracting the number of cards remaining in the cols and reserves from 52.
score :: EOBoard -> Int
score (founds, cols, res) = 52 - (length res) - (foldr ((+) . length) 0 cols)

-- Maybe helpers
isJust :: (Maybe a) -> Bool
isJust (Just _) = True
isJust Nothing = False

resMaybe :: (Maybe a) -> a
resMaybe (Just x) = x
