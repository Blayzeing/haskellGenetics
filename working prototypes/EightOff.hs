{- |
Module      :  EightOff
Description :  An implementation of a basic Eightoff game with utility functions and an autoplay system, for assignment 2 of COM2001
Author      :  Blayze Millward
-}
module EightOff where -- Define the module
import System.Random -- Import System.Random for shuffling
import Data.List -- Import Data.List to gain access to `sortBy` and `delete`
import Debug.Trace

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
--eODeal :: Int -> EOBoard
--eODeal seed = ([], cols, res) -- Construct the board with an empty foundations and use the variables defined below to place the deck's cards.
--    where
--        deck = shuffle seed -- Creates a randomly shuffled (based on the given seed) deck to deal.
--        -- The comprehension below splits the first 48 cards of a deck into 8 6-long columns, by taking every card up to the end of,
--        -- then dropping every card before the beginning of each column and forming a list:
--        cols = [drop (n*6) (take (n*6 + 6) deck) | n <- [0..7]]
--        res = drop 48 deck -- Finally, take the final 4 cards from the deck to form the cards in the reserves
eODeal :: Deck -> EOBoard
eODeal deck = ([], cols, res)
    where
        cols = [drop (n*6) (take (n*6 + 6) deck) | n <- [0..7]]
        res = drop 48 deck

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


--findMoves :: EOBoard -> [EOBoard]
--findMoves board
--    | (not . null) kingMoves = kingMoves
--    | otherwise = otherMoves
--    where
--        kingMoves = findKingMoves board
--        otherMoves = findNonKingMoves board -- convertToMoves(findColumnHeadMoves board)

-- The below should be just as good as the above
findMoves :: EOBoard -> [EOBoard]
findMoves board = (findKingMoves board) ++ (findNonKingMoves board)

-- This function finds all king moves, namely all moves that insert a king into a new column
findKingMoves :: EOBoard -> [EOBoard]
findKingMoves board@(founds, cols, res)
    | length cols == 8 = [] -- return nothing if there are no null columns for kings to move to
    -- If there are kings that can move, return a list of moves that can be made with them:
    | (not . null) kingCards = map (\ n -> (founds, [n]:(map (delete n) cols), delete n res)) kingCards 
    | otherwise = [] -- If no king moves were found, then return an empty list
    -- King cards are all kings in moveable places (res and column heads), excluding from columns of size 1, where if they are a king, they should stay:
    where kingCards = filter isKing (res ++ (getColHeads (filter (\ n -> length n > 1) cols)))

-- This function finds all non-king moves, namely all moves to the heads of columns
findNonKingMoves :: EOBoard -> [EOBoard]
findNonKingMoves board@(founds, cols, res) = map fst (sortBy (\(_,n1) (_,n2) -> compare n1 n2) unsortedMoves) -- Sort all potential moves by rank
    where
        bonusCards = getRequiredCards board -- Store a list of cards toFoundatios needs, if these are found, they are worth more
        neededCards = map pCard (filter (not . isAce) (getColHeads cols)) -- List of cards to look for
        rankedCards = map (rank bonusCards) neededCards -- A list of cards ranked to show if they are more desired.
        --rankedCards = (map (\ n -> (n,-1)) (filter (elem bonusCards) neededCards)) ++ (map (\ n -> (n,0)) (filter ((not . elem) bonusCards) neededCards))
        unsortedMoves = map resMaybe (filter isJust (map (convertToMove board) rankedCards)) -- Convert all ranked cards into moves, strip out illegal ones

-- This function ranks individual cards based on whether they appear in a list of cards
rank :: [Card] -> Card -> (Card, Int)
rank cards card
    | elem card cards = (card, -1)
    | otherwise = (card, 0)

-- Takes a ranked card and a board and converts it into a ranked move.
convertToMove :: EOBoard -> (Card, Int) -> Maybe (EOBoard, Int)
convertToMove board@(founds, cols, res) (card, rank)
    -- If the card is in the rserves, it is a one-turn move, so keep the rank the same and remove it from res and add it to cols:
    -- As moving a card out of the reserves is more important than between columns, give this move a more important rank
    | elem card res = Just ((founds, addToColumns cols card, delete card res), rank - 1)
    -- Again, if the card is on the head of a column, it's also one-turn move; remove it from the columns and add it in the correct place:
    | elem card colHeads = Just ((founds, addToColumns (map (delete card) cols) card, res), rank) 
    -- If the top cards are properly stacked, do not move them. Doing so would create loops:
    -- | properlyPlaced card cols = Nothing
    -- If the card is in a column (and a reserve slot is free), calculate it's reserve heuristic, and move the top card from the column to the resrve:
    | length res < 8 && (foldr ((||) . elem card) False cols) = Just ((founds, map (delete topCard) cols, topCard:res), rank + distanceHeuristic)
    | otherwise = Nothing -- If the card cannot be found, or there are no places left in reserve to move a card to, return nothing
    where
        colHeads = getColHeads cols 
        -- distHeuristic takes 'card' and calculates the number of moves it would take to move it to the top of it's column
        distanceHeuristic = resMaybe $ head $ filter isJust $ map (elemIndex card) cols -- Note: This is only called if there is a head
        -- topCard stores the card at the top of the column in which 'card' is in:
        topCard = head $ foldr (\ col@(h:t) r -> if elem card col then h:r else r) [] cols -- Note: Also only called if there is a head

-- Function to calculate if a card has been properly placed
--properlyPlaced :: Card -> Columns -> Bool
--properlyPlaced c cols
--    | null containingCols = False
--    | otherwise = True
--    where containingCols = filter (\ col -> length col > 1 && elem c col && not (isKing (col!!0)) && sCard(col!!0) == col!!1) cols

-- Function to add a given card to a set of columns, in the appropriate position
addToColumns :: Columns -> Card -> Columns
addToColumns cols card = map (insertCard card) populatedCols
    where
        populatedCols = filter (not . null) cols -- Get rid of null columns
        insertCard :: Card -> [Card] -> [Card]
        insertCard card col@(h:t)
            | sCard card == h = card:col
            | otherwise = col

-- A helper function to get the heads of all columns and all cards in reserves (all moveable cards)
getColHeads :: Columns -> [Card]
getColHeads cols = map head (filter (not . null) cols)


thrd (a,b,c) = c


chooseMove :: EOBoard -> Maybe EOBoard
chooseMove board
    | null moves = Nothing
    | otherwise = {-traceShow (toFoundations (head moves)) $-} Just (toFoundations (head moves))
    where
        moves = findMoves board
        -- The goal of below is to go through every unsafe move and remove it if applything the move results in the same set of moves
        -- (basically, if applying this move will result in a loop, then don't do it)
--        moves = unsafeMoves --filter isNonCyclic unsafeMoves
        --moves = filter (\ move -> findMoves(toFoundations(head findMoves(toFoundations (move)))) /= unsafeMoves) unsafeMoves -- Strip out any loops

--isNonCyclic :: EOBoard -> Bool
--isNonCyclic move
--    | null oneAhead || null twoAhead || null threeAhead = True
--    | head oneAhead == move || head twoAhead == move || head threeAhead == move = False
--    | otherwise = True
--    where
--        oneAhead = findMoves(toFoundations(move))
--        twoAhead = findMoves(toFoundations(head oneAhead))
--        threeAhead = findMoves( toFoundations(head twoAhead))

eOGame :: EOBoard -> Int -- Takes an initial board, then plays the game, returning a final score
eOGame board
    | isJust nextMove = eOGame(resMaybe nextMove) -- Play the next move with the update board state
    | otherwise = score board -- There is no next move, just return this board's score
    where nextMove = chooseMove board -- Calculate the next board state

-- Calculate the score of a given EOBoard, this is done by subtracting the number of cards remaining in the cols and reserves from 52.
score :: EOBoard -> Int
score (founds, cols, res) = 52 - (length res) - (foldr ((+) . length) 0 cols)

isJust :: (Maybe a) -> Bool
isJust (Just _) = True
isJust Nothing = False

resMaybe :: (Maybe a) -> a
resMaybe (Just x) = x
